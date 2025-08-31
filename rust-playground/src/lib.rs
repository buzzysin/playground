//! rust_playground: small crate implementing plugs, sockets and an adaptor.
#![deny(missing_docs)]

/// Adaptor trait: convert an `O` into an `I`.
pub trait IAdaptor<I, O> {
    /// Adapt `input` (O) into an `I`.
    fn adapt(&self, input: O) -> I;
}

/// 2-pin plug (live, neutral)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TwoPinPlug {
    /// Live pin voltage
    pub live: i32,
    /// Neutral pin voltage
    pub neutral: i32,
}

impl TwoPinPlug {
    /// Create a new TwoPinPlug
    pub fn new(live: i32, neutral: i32) -> Self {
        Self { live, neutral }
    }
}

/// 3-pin plug (live, neutral, ground)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThreePinPlug {
    /// Live pin voltage
    pub live: i32,
    /// Neutral pin voltage
    pub neutral: i32,
    /// Ground pin (earth)
    pub ground: i32,
}

impl ThreePinPlug {
    /// Create a new ThreePinPlug
    pub fn new(live: i32, neutral: i32, ground: i32) -> Self {
        Self {
            live,
            neutral,
            ground,
        }
    }
}

/// Two-pin socket: accepts TwoPinPlug
#[derive(Debug, Default)]
pub struct TwoPinSocket;

impl TwoPinSocket {
    /// Accept returns `true` if the plug "matches" the socket in a simple sense.
    /// Here we consider non-zero pins to be valid.
    pub fn accept(&self, plug: &TwoPinPlug) -> bool {
        plug.live != 0 && plug.neutral != 0
    }
}

/// Three-pin socket: accepts ThreePinPlug
#[derive(Debug, Default)]
pub struct ThreePinSocket;

impl ThreePinSocket {
    /// Accept returns true when live, neutral and ground are non-zero.
    pub fn accept(&self, plug: &ThreePinPlug) -> bool {
        plug.live != 0 && plug.neutral != 0 && plug.ground != 0
    }
}

/// Adaptor that allows a ThreePinPlug to be adapted into a TwoPinPlug
/// by discarding/ignoring the ground pin.
#[derive(Debug, Default)]
pub struct ThreeToTwoPinSocketAdaptor;

impl IAdaptor<TwoPinPlug, ThreePinPlug> for ThreeToTwoPinSocketAdaptor {
    fn adapt(&self, input: ThreePinPlug) -> TwoPinPlug {
        TwoPinPlug::new(input.live, input.neutral)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn two_pin_socket_accepts_two_pin_plug() {
        let socket = TwoPinSocket;
        let plug = TwoPinPlug::new(1, -1);
        assert!(socket.accept(&plug));
    }

    #[test]
    fn three_pin_socket_accepts_three_pin_plug() {
        let socket = ThreePinSocket;
        let plug = ThreePinPlug::new(1, -1, 0); // missing ground -> reject
        assert!(!socket.accept(&plug));
        let plug2 = ThreePinPlug::new(1, -1, 1);
        assert!(socket.accept(&plug2));
    }

    #[test]
    fn adaptor_converts_three_to_two_and_two_socket_accepts() {
        let three = ThreePinPlug::new(1, -1, 1);
        let adaptor = ThreeToTwoPinSocketAdaptor;
        let two = adaptor.adapt(three);
        let socket = TwoPinSocket;
        assert!(socket.accept(&two));
    }
}
