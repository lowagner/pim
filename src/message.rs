use crate::color;
use crate::gfx::Rgba8;

use std::fmt;

/// A message to the user, displayed in the session.
#[derive(PartialEq, Debug, Clone)]
pub struct Message {
    /// The message string.
    pub string: String,
    /// The message type.
    pub message_type: MessageType,
}

impl Message {
    /// Create a new message.
    pub fn new<D: fmt::Display>(s: D, t: MessageType) -> Self {
        Message {
            string: format!("{}", s),
            message_type: t,
        }
    }

    /// Return the color of a message.
    pub fn color(&self) -> Rgba8 {
        self.message_type.color()
    }

    pub fn is_execution(&self) -> bool {
        self.message_type == MessageType::Execution
    }

    pub fn is_debug(&self) -> bool {
        self.message_type == MessageType::Debug
    }

    /// Log a message to stdout/stderr.
    pub fn log(&self) {
        if self.string.is_empty() {
            return;
        }
        match self.message_type {
            MessageType::Info => info!("{}", self),
            MessageType::Hint => {}
            MessageType::Echo => info!("{}", self),
            MessageType::Error => error!("{}", self),
            MessageType::Warning => warn!("{}", self),
            MessageType::Execution => {}
            MessageType::Okay => info!("{}", self),
            MessageType::Debug => debug!("{}", self),
        }
    }
}

impl Default for Message {
    fn default() -> Self {
        Message::new("", MessageType::Info)
    }
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.string.fmt(f)
    }
}

/// The type of a `Message`.
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum MessageType {
    /// A hint that can be ignored.
    Hint,
    /// Informational message.
    Info,
    /// A message that is displayed by the `:echo` command.
    Echo,
    /// An error message.
    Error,
    /// Non-critical warning.
    Warning,
    /// Execution-related message.
    Execution,
    /// Debug message.
    Debug,
    /// Success message.
    Okay,
}

impl MessageType {
    /// Returns the color associated with a `MessageType`.
    fn color(self) -> Rgba8 {
        match self {
            MessageType::Info => color::LIGHT_GREY,
            MessageType::Hint => color::DARK_GREY,
            MessageType::Echo => color::LIGHT_GREEN,
            MessageType::Error => color::RED,
            MessageType::Warning => color::YELLOW,
            MessageType::Execution => color::GREY,
            MessageType::Debug => color::LIGHT_GREEN,
            MessageType::Okay => color::GREEN,
        }
    }
}
