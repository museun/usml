use crate::{span::Span, util::count_digits};
use std::ops::Range;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Level {
    Warn,
    Error,
    Bug,
}

#[derive(Debug)]
pub struct Annotation {
    pub span: Span,
    pub info: String,
}

impl Annotation {
    pub fn new(span: Span, info: impl Into<String>) -> Self {
        Self {
            span,
            info: info.into(),
        }
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub level: Level,
    pub primary: Annotation,
    pub info: Vec<String>,
    pub other: Vec<Annotation>,
}

impl Diagnostic {
    const fn from_annotation(level: Level, primary: Annotation) -> Self {
        Self {
            level,
            primary,
            info: Vec::new(),
            other: Vec::new(),
        }
    }

    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self::from_annotation(Level::Error, Annotation::new(span, message))
    }

    pub fn warn(message: impl Into<String>, span: Span) -> Self {
        Self::from_annotation(Level::Warn, Annotation::new(span, message))
    }

    pub fn bug(message: impl Into<String>, span: Span) -> Self {
        Self::from_annotation(Level::Bug, Annotation::new(span, message))
    }

    pub fn message(mut self, span: Span, message: impl Into<String>) -> Self {
        self.other.push(Annotation::new(span, message));
        self
    }

    pub fn info(mut self, message: impl Into<String>) -> Self {
        self.info.push(message.into());
        self
    }

    pub fn lines_range(&self) -> Range<u16> {
        let mut range = self.primary.span.start.line..self.primary.span.end.line;

        for other in &self.other {
            range.start = std::cmp::min(other.span.start.line, range.start);
            range.end = std::cmp::max(other.span.end.line + 1, range.end)
        }

        range
    }

    pub fn render(self, source: &str, verbosity: Verbosity) -> String {
        match verbosity {
            Verbosity::Verbose(0) => self.minimal(source),
            Verbosity::Verbose(..) => self.verbose(source),
        }
    }

    pub fn minimal(mut self, _source: &str) -> String {
        let mut output = format!("{:?}\n", self.level);
        self.other.insert(0, self.primary);

        for Annotation { span, info } in self.other {
            if span == Span::unknown() {
                output.push_str(&info);
                output.push('\n');
                continue;
            }
            output.push_str(&format!(
                "{},{} {}\n",
                span.start.line + 1,
                span.start.col + 1,
                info
            ));
        }

        output
    }

    pub fn verbose(mut self, source: &str) -> String {
        let lines = source.lines().collect::<Vec<_>>();

        let mut output = format!("{:?}\n", self.level);
        self.other.insert(0, self.primary);

        for Annotation { span, info } in self.other {
            if span == Span::unknown() {
                output.push_str(&info);
                output.push('\n');
                continue;
            }

            let range = span.start.line.saturating_sub(1)..span.end.line + 1;
            let width = count_digits((span.end.line + 1) as _);
            for n in range {
                output.push_str(&format!("{:>.*} | {}\n", width, n + 1, lines[n as usize]));
                if span.start.line != n {
                    continue;
                }

                let empty = " ".repeat((span.start.col as usize) + 3 + width);
                let line = "^".repeat(span.end.col.saturating_sub(span.start.col + 2) as usize);
                output.push_str(&format!("{}^{}^ {}\n", empty, line, info))
            }

            if !self.info.is_empty() {
                output.push('\n');
            }

            self.info.iter().for_each(|s| {
                output.push_str("note: ");
                output.push_str(s);
                output.push('\n')
            });
        }

        output
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Verbosity {
    Verbose(u8),
}

macro_rules! diag {
    ($span:expr) => {
        compiler_error!("not yet")
    };
    ($span:expr, $fmt:expr, $($args:expr),* $(,)?) => {
        Diagnostic::error(format!($fmt, $($args),*), $span)
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Position;
    #[test]
    fn diag() {
        let input = "hello world this is a test";

        let diag = Diagnostic::warn(
            "assumption of location",
            Span::new(Position::new(0, 6), Position::new(0, 11)),
        )
        .info("some data can go here")
        .info("and more for reference");

        eprintln!("{}", diag.render(input, Verbosity::Verbose(2)));
    }
}
