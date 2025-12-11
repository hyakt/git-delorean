use std::{
  env, io,
  path::{Component, Path, PathBuf},
  process::Command,
  time::{Duration, Instant},
};

use anyhow::{Context, Result, anyhow, bail};
use crossterm::{
  event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
  execute,
  terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use once_cell::sync::{Lazy, OnceCell};
use ratatui::{
  layout::{Constraint, Direction, Layout},
  prelude::*,
  text::{Line, Span},
  widgets::{Block, Paragraph, Wrap},
};
use syntect::{
  easy::HighlightLines,
  highlighting::{Style as SynStyle, Theme, ThemeSet},
  parsing::{SyntaxReference, SyntaxSet},
  util::LinesWithEndings,
};
use unicode_width::UnicodeWidthStr;

#[derive(Debug)]
struct CommitEntry {
  date: String,
  author: String,
  subject: String,
  short_hash: String,
  hash: String,
  path: PathBuf,
  content: FileContent,
  spans: OnceCell<Vec<Line<'static>>>,
}

#[derive(Debug)]
enum FileContent {
  Text(String),
  Binary(usize),
}

#[derive(Debug)]
enum UserCommand {
  Next,
  Prev,
  OpenPr,
  ScrollDown,
  ScrollUp,
  PageDown,
  PageUp,
  Quit,
  None,
}

#[derive(Debug)]
struct AppState {
  entries: Vec<CommitEntry>,
  index: usize,
  scroll: usize,
  status: Option<String>,
  status_until: Option<Instant>,
  cursor_row: usize,
}

fn set_status(app: &mut AppState, msg: impl Into<String>, secs: u64) {
  app.status = Some(msg.into());
  app.status_until = Some(Instant::now() + Duration::from_secs(secs));
}

static SYNTAX_SET: Lazy<SyntaxSet> = Lazy::new(SyntaxSet::load_defaults_newlines);
static THEME_SET: Lazy<ThemeSet> = Lazy::new(ThemeSet::load_defaults);
static THEME: Lazy<Theme> = Lazy::new(|| pick_theme(&THEME_SET));

struct TerminalGuard;

impl TerminalGuard {
  fn enter() -> Result<Self> {
    enable_raw_mode()?;
    execute!(io::stdout(), EnterAlternateScreen)?;
    Ok(Self)
  }
}

impl Drop for TerminalGuard {
  fn drop(&mut self) {
    let _ = disable_raw_mode();
    let _ = execute!(io::stdout(), LeaveAlternateScreen);
  }
}

fn main() -> Result<()> {
  let mut args = env::args().skip(1);
  let file_arg = args
    .next()
    .ok_or_else(|| anyhow!("Usage: git-delorean <file>"))?;
  if args.next().is_some() {
    bail!("Usage: git-delorean <file>");
  }

  let target_path = normalize_path(Path::new(&file_arg))?;
  let repo_root = repo_root_for(&target_path)?;
  let rel_path = target_path
    .strip_prefix(&repo_root)
    .map(Path::to_path_buf)
    .map_err(|_| anyhow!("Path must be inside git repo: {}", target_path.display()))?;

  let entries = build_history(&repo_root, &rel_path)?;
  if entries.is_empty() {
    bail!("No commits found for {}", rel_path.to_string_lossy());
  }

  let _term = TerminalGuard::enter()?;
  let mut terminal = ratatui::Terminal::new(ratatui::backend::CrosstermBackend::new(io::stdout()))?;

  let mut app = AppState {
    entries,
    index: 0,
    scroll: 0,
    status: None,
    status_until: None,
    cursor_row: 0,
  };

  loop {
    if let Some(until) = app.status_until {
      if Instant::now() >= until {
        app.status = None;
        app.status_until = None;
      }
    }

    let size = terminal.get_frame().size();
    let body_height = body_height(size.height);
    adjust_scroll(&mut app, body_height);

    terminal.draw(|f| draw_app(f, &app, body_height, size.width))?;

    match read_single_key()? {
      UserCommand::Next => {
        if app.index > 0 {
          app.index -= 1;
        } else {
          set_status(&mut app, "Already at the newest revision.", 2);
        }
      }
      UserCommand::Prev => {
        if app.index + 1 < app.entries.len() {
          app.index += 1;
        } else {
          set_status(&mut app, "Already at the oldest revision.", 2);
        }
      }
      UserCommand::OpenPr => match open_pr(&repo_root, &app.entries[app.index].hash) {
        Ok(msg) => {
          if let Some(m) = msg {
            set_status(&mut app, m, 2);
          } else {
            set_status(&mut app, "PR opened in browser.", 2);
          }
        }
        Err(e) => {
          set_status(&mut app, format!("PR open failed: {}", e), 2);
        }
      },
      UserCommand::ScrollDown => {
        move_cursor_line(&mut app, body_height, 1);
      }
      UserCommand::ScrollUp => {
        move_cursor_line(&mut app, body_height, -1);
      }
      UserCommand::PageDown => {
        let max_off = max_scroll(total_lines(&app.entries[app.index]), body_height);
        app.scroll = (app.scroll + body_height as usize).min(max_off);
        app.cursor_row = (body_height as usize).saturating_sub(1);
      }
      UserCommand::PageUp => {
        app.scroll = app.scroll.saturating_sub(body_height as usize);
        app.cursor_row = 0;
      }
      UserCommand::Quit => break,
      UserCommand::None => {}
    }
  }

  Ok(())
}

fn draw_app(f: &mut Frame, app: &AppState, body_height: u16, _width: u16) {
  let chunks = Layout::default()
    .direction(Direction::Vertical)
    .constraints([
      Constraint::Length(1),
      Constraint::Min(1),
      Constraint::Length(1),
    ])
    .split(f.size());

  let entry = &app.entries[app.index];
  let idx_span = Span::styled(
    format!("[{}/{}] ", app.index + 1, app.entries.len()),
    Style::default().fg(Color::Rgb(160, 160, 160)),
  );
  let author_span = Span::styled(
    entry.author.clone(),
    Style::default().fg(Color::Rgb(255, 165, 0)),
  );
  let subject_span = Span::styled(
    entry.subject.clone(),
    Style::default().fg(Color::Rgb(255, 215, 0)),
  );
  let date_span = Span::styled(
    format_datetime(&entry.date),
    Style::default().fg(Color::Rgb(128, 189, 255)),
  );
  let hash_span = Span::styled(
    format!("({})", entry.short_hash),
    Style::default().fg(Color::Rgb(160, 160, 160)),
  );
  let header_line = Line::from(vec![
    idx_span,
    author_span,
    Span::raw(": "),
    subject_span,
    Span::raw(" "),
    date_span,
    Span::raw(" "),
    hash_span,
  ]);
  let spans = rendered_lines(entry);
  let body_width = chunks[1].width as usize;
  let (view, view_len) =
    spans_to_lines_with_cursor(spans, app.scroll, body_height, app.cursor_row, body_width);
  let body_widget = Paragraph::new(view).wrap(Wrap { trim: false });

  let header_bg = Color::Rgb(12, 12, 12);
  let header_widget =
    Paragraph::new(header_line).block(Block::default().style(Style::default().bg(header_bg)));

  let commands = "Keys: n(next) p(prev) o(open PR) q(quit) | Scroll: j/k";
  let footer_text = app
    .status
    .clone()
    .filter(|s| !s.is_empty())
    .unwrap_or_else(|| commands.into());
  let footer_bg = Color::Rgb(8, 8, 8);
  let footer_widget =
    Paragraph::new(footer_text).block(Block::default().style(Style::default().bg(footer_bg)));

  f.render_widget(header_widget, chunks[0]);
  f.render_widget(body_widget, chunks[1]);
  if view_len > 0 {
    let rel = app.cursor_row.min(view_len.saturating_sub(1));
    f.set_cursor(chunks[1].x, chunks[1].y + rel as u16);
  }
  f.render_widget(footer_widget, chunks[2]);

  // Adjust body area height if terminal resized; already passed in.
  let _ = body_height;
}

fn highlight_to_spans(text: &str, path: &Path) -> Vec<Line<'static>> {
  let syntax = syntax_for(path, text);
  let mut h = HighlightLines::new(syntax, &THEME);

  let mut lines = Vec::new();
  for line in LinesWithEndings::from(text) {
    let display = line.trim_end_matches(&['\n', '\r']);
    match h.highlight_line(line, &SYNTAX_SET) {
      Ok(hl) => {
        let spans: Vec<_> = hl
          .into_iter()
          .map(|(style, piece)| Span::styled(piece.to_string(), style_to_tui(style)))
          .collect();
        lines.push(Line::from(spans));
      }
      Err(_) => {
        // Fallback: plain line if highlight fails
        lines.push(Line::from(display.to_string()));
      }
    };
  }
  if lines.is_empty() {
    lines.push(Line::from(""));
  }
  lines
}

fn rendered_lines(entry: &CommitEntry) -> &Vec<Line<'static>> {
  entry.spans.get_or_init(|| match &entry.content {
    FileContent::Text(text) => highlight_to_spans(text, &entry.path),
    FileContent::Binary(size) => vec![Line::from(format!("[binary file: {} bytes]", size))],
  })
}

fn spans_to_lines_with_cursor(
  spans: &[Line<'static>],
  scroll: usize,
  body_height: u16,
  cursor_row: usize,
  body_width: usize,
) -> (Vec<Line<'static>>, usize) {
  let start = scroll;
  let end = (scroll + body_height as usize).min(spans.len());
  let view_len = end.saturating_sub(start);
  let view = spans
    .iter()
    .enumerate()
    .skip(start)
    .take(view_len)
    .map(|(i, line)| {
      let mut line = line.clone();
      if i - start == cursor_row {
        // やや明るめの #2A2723 で現在行を強調。
        let cursor_style = Style::default().bg(Color::Rgb(42, 39, 35));
        line.style = line.style.patch(cursor_style);
        line.spans = line
          .spans
          .into_iter()
          .map(|span| {
            let mut span = span;
            span.style = span.style.patch(cursor_style);
            span
          })
          .collect();
        // 行末以降も背景が途切れないように右端まで空白を追加。
        let current_width: usize = line.spans.iter().map(|s| s.content.width()).sum();
        if body_width > current_width {
          let pad = " ".repeat(body_width - current_width);
          line.spans.push(Span::styled(pad, cursor_style));
        }
      }
      line
    })
    .collect();
  (view, view_len)
}

fn style_to_tui(style: SynStyle) -> Style {
  let fg = style.foreground;
  // Ignore theme backgrounds so it blends with the terminal background.
  Style::default()
    .fg(Color::Rgb(fg.r, fg.g, fg.b))
    .bg(Color::Reset)
}

fn pick_theme(set: &ThemeSet) -> Theme {
  set
    .themes
    .get("InspiredGitHub")
    .cloned()
    .unwrap_or_else(|| Theme::default())
}

fn format_datetime(iso: &str) -> String {
  if iso.len() >= 16 {
    let date = iso[0..10].replace('-', "/");
    let time = &iso[11..16];
    format!("{date} {time}")
  } else {
    iso.to_string()
  }
}

fn syntax_for<'a>(path: &Path, text: &'a str) -> &'a SyntaxReference {
  let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
  let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");

  let by_ext = SYNTAX_SET.find_syntax_by_extension(ext);
  if let Some(s) = by_ext {
    return s;
  }

  if let Some(s) = SYNTAX_SET.find_syntax_by_token(name) {
    return s;
  }

  if let Some(candidates) = mapped_syntax_name(ext) {
    for candidate in candidates {
      if let Some(s) = SYNTAX_SET.find_syntax_by_name(candidate) {
        return s;
      }
    }
  }

  if let Some(s) = SYNTAX_SET.find_syntax_by_first_line(text) {
    return s;
  }

  SYNTAX_SET.find_syntax_plain_text()
}

fn mapped_syntax_name(ext: &str) -> Option<&'static [&'static str]> {
  Some(match ext.to_ascii_lowercase().as_str() {
    "ts" => &["TypeScript", "JavaScript"],
    "tsx" => &["TypeScriptReact", "JavaScript (JSX)", "JavaScript"],
    "js" | "cjs" | "mjs" => &["JavaScript"],
    "jsx" => &["JavaScript (JSX)", "JavaScript"],
    "sh" | "bash" | "zsh" => &["ShellScript"],
    "rs" => &["Rust"],
    "py" => &["Python"],
    "go" => &["Go"],
    "rb" => &["Ruby"],
    "css" => &["CSS"],
    "html" | "htm" => &["HTML"],
    "json" | "jsonc" => &["JSON"],
    "yml" | "yaml" => &["YAML"],
    "toml" => &["TOML"],
    _ => return None,
  })
}

fn read_single_key() -> Result<UserCommand> {
  loop {
    match event::read()? {
      Event::Key(key) if key.kind == KeyEventKind::Press => match key.code {
        // Ignore IME確定などで飛んでくる非ASCIIキー。
        KeyCode::Char(c) if !c.is_ascii() => return Ok(UserCommand::None),
        KeyCode::Char('n') if key.modifiers.contains(KeyModifiers::CONTROL) => {
          return Ok(UserCommand::ScrollDown);
        }
        KeyCode::Char('p') if key.modifiers.contains(KeyModifiers::CONTROL) => {
          return Ok(UserCommand::ScrollUp);
        }
        KeyCode::Char('n') => return Ok(UserCommand::Next),
        KeyCode::Char('p') => return Ok(UserCommand::Prev),
        KeyCode::Char('o') => return Ok(UserCommand::OpenPr),
        KeyCode::Char('j') | KeyCode::Down => return Ok(UserCommand::ScrollDown),
        KeyCode::Char('k') | KeyCode::Up => return Ok(UserCommand::ScrollUp),
        KeyCode::Char('f') | KeyCode::PageDown => return Ok(UserCommand::PageDown),
        KeyCode::Char('b') | KeyCode::PageUp => return Ok(UserCommand::PageUp),
        KeyCode::Char('q') => return Ok(UserCommand::Quit),
        _ => return Ok(UserCommand::None),
      },
      _ => {}
    }
  }
}

fn adjust_scroll(app: &mut AppState, body_height: u16) {
  let total = total_lines(&app.entries[app.index]);
  let max_off = max_scroll(total, body_height);
  if app.scroll > max_off {
    app.scroll = max_off;
  }
  // Clamp cursor row so it stays within the visible area.
  let visible_lines = (body_height as usize).min(total.saturating_sub(app.scroll));
  if visible_lines == 0 {
    app.cursor_row = 0;
  } else {
    app.cursor_row = app.cursor_row.min(visible_lines - 1);
  }
}

fn total_lines(entry: &CommitEntry) -> usize {
  rendered_lines(entry).len().max(1)
}

fn body_height(terminal_height: u16) -> u16 {
  // header 1 line + footer 1 line
  let reserved = 2;
  terminal_height.saturating_sub(reserved).max(1)
}

fn max_scroll(total_lines: usize, body_height: u16) -> usize {
  total_lines.saturating_sub(body_height as usize)
}

fn move_cursor_line(app: &mut AppState, body_height: u16, delta: isize) {
  let total = total_lines(&app.entries[app.index]);
  if total == 0 {
    return;
  }

  let current_abs = app.scroll + app.cursor_row;
  let target_abs = if delta > 0 {
    let step = delta as usize;
    let target = current_abs + step;
    if target >= total {
      return;
    }
    target
  } else {
    let step = delta.unsigned_abs() as usize;
    if current_abs < step {
      return;
    }
    current_abs - step
  };

  let body_height = body_height as usize;
  let visible_end = app.scroll + body_height.saturating_sub(1);
  if target_abs > visible_end {
    app.scroll = target_abs + 1 - body_height;
  } else if target_abs < app.scroll {
    app.scroll = target_abs;
  }

  let max_off = max_scroll(total, body_height as u16);
  if app.scroll > max_off {
    app.scroll = max_off;
  }

  app.cursor_row = target_abs.saturating_sub(app.scroll);
}

fn repo_root_for(path: &Path) -> Result<PathBuf> {
  let anchor = if path.is_dir() {
    path
  } else {
    path.parent().unwrap_or(Path::new("/"))
  };
  let output = git_output(&["rev-parse", "--show-toplevel"], Some(anchor))?;
  Ok(PathBuf::from(output.trim()))
}

fn normalize_path(input: &Path) -> Result<PathBuf> {
  let joined = if input.is_absolute() {
    input.to_path_buf()
  } else {
    env::current_dir()
      .context("Failed to get current directory")?
      .join(input)
  };

  let mut normalized = PathBuf::new();
  for comp in joined.components() {
    match comp {
      Component::CurDir => {}
      Component::ParentDir => {
        normalized.pop();
      }
      Component::RootDir | Component::Normal(_) => normalized.push(comp.as_os_str()),
      Component::Prefix(prefix) => normalized.push(prefix.as_os_str()),
    }
  }
  Ok(normalized)
}

fn build_history(repo_root: &Path, path: &Path) -> Result<Vec<CommitEntry>> {
  let path_str = path
    .to_str()
    .ok_or_else(|| anyhow!("Path is not valid UTF-8"))?;

  let log_output = git_output(
    &[
      "log",
      "--follow",
      "--name-status",
      "--date=iso-strict",
      "--abbrev=8",
      "--format=%H%x1f%h%x1f%ad%x1f%an%x1f%s",
      "--",
      path_str,
    ],
    Some(repo_root),
  )?;

  let mut lines = log_output.lines().peekable();
  let mut entries = Vec::new();
  let mut current_path = path.to_path_buf();

  while let Some(line) = lines.next() {
    if line.trim().is_empty() {
      continue;
    }

    if !line.contains('\u{1f}') {
      continue;
    }
    let parts: Vec<_> = line.split('\u{1f}').collect();
    if parts.len() < 5 {
      continue;
    }
    let hash = parts[0];
    let short = parts[1];
    let date = parts[2].to_string();
    let author = parts[3].to_string();
    let subject = parts[4].to_string();

    let mut status_lines = Vec::new();
    while let Some(next) = lines.peek() {
      if next.trim().is_empty() {
        lines.next();
        break;
      }
      if next.contains('\u{1f}') {
        break;
      }
      status_lines.push(lines.next().unwrap().to_string());
    }

    let deleted = status_lines
      .iter()
      .any(|s| is_deletion_line(s, &current_path));
    let commit_path = current_path.clone();

    if !deleted {
      if let Some(content) = read_file_at_commit(repo_root, hash, &commit_path)? {
        entries.push(CommitEntry {
          date,
          author,
          subject,
          short_hash: short.to_string(),
          hash: hash.to_string(),
          path: commit_path.clone(),
          content,
          spans: OnceCell::new(),
        });
      }
    }

    if let Some(previous_path) = find_rename(&status_lines, &current_path) {
      current_path = previous_path;
    }
  }

  Ok(entries)
}

fn is_deletion_line(line: &str, path: &Path) -> bool {
  if !line.starts_with('D') {
    return false;
  }
  let mut parts = line.split('\t').skip(1);
  if let Some(file) = parts.next() {
    return Path::new(file) == path;
  }
  false
}

fn find_rename(status_lines: &[String], current_path: &Path) -> Option<PathBuf> {
  for line in status_lines {
    if !line.starts_with('R') {
      continue;
    }
    let mut parts = line.split('\t').skip(1);
    if let (Some(old_path), Some(new_path)) = (parts.next(), parts.next()) {
      let new_path_buf = PathBuf::from(new_path);
      if new_path_buf == current_path {
        return Some(PathBuf::from(old_path));
      }
    }
  }
  None
}

fn read_file_at_commit(repo_root: &Path, hash: &str, path: &Path) -> Result<Option<FileContent>> {
  let spec = format!("{}:{}", hash, path.to_string_lossy());
  let output = Command::new("git")
    .args(["show", &spec])
    .current_dir(repo_root)
    .output()
    .with_context(|| format!("Failed to read {spec}"))?;
  if !output.status.success() {
    return Ok(None);
  }

  let bytes = output.stdout;
  let is_binary = bytes.iter().any(|b| *b == 0);

  if is_binary {
    return Ok(Some(FileContent::Binary(bytes.len())));
  }

  let text = String::from_utf8_lossy(&bytes).to_string();
  Ok(Some(FileContent::Text(text)))
}

fn open_pr(repo_root: &Path, base_hash: &str) -> Result<Option<String>> {
  let range = format!("{base_hash}..HEAD");
  let log = git_output(
    &[
      "log",
      "--merges",
      "--oneline",
      "--reverse",
      "--ancestry-path",
      &range,
    ],
    Some(repo_root),
  )?;

  let mut pr_number: Option<String> = None;
  for line in log.lines() {
    if let Some(idx) = line.find("Merge pull request #") {
      let rest = &line[idx + "Merge pull request #".len()..];
      let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
      if !digits.is_empty() {
        pr_number = Some(digits);
        break;
      }
    }
  }

  if let Some(pr) = pr_number {
    let status = Command::new("gh")
      .args(["pr", "view", "--web", &pr])
      .current_dir(repo_root)
      .status()
      .with_context(|| "Failed to run gh")?;
    if status.success() {
      Ok(None)
    } else {
      bail!("gh pr view exited with {}", status);
    }
  } else {
    Ok(Some("No PR found on ancestry path.".into()))
  }
}

fn git_output(args: &[&str], cwd: Option<&Path>) -> Result<String> {
  let mut cmd = Command::new("git");
  cmd.args(args);
  if let Some(dir) = cwd {
    cmd.current_dir(dir);
  }
  let output = cmd
    .output()
    .with_context(|| format!("Failed to run git {}", args.join(" ")))?;
  if !output.status.success() {
    bail!(
      "git {} failed: {}",
      args.join(" "),
      String::from_utf8_lossy(&output.stderr)
    );
  }
  Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
