use rust_playground::{
    IAdaptor, ThreePinPlug, ThreePinSocket, ThreeToTwoPinSocketAdaptor, TwoPinSocket,
};

fn main() {
    let three_plug = ThreePinPlug::new(230, -230, 1);
    let three_socket = ThreePinSocket;
    println!(
        "Three socket accepts three plug? {}",
        three_socket.accept(&three_plug)
    );

    // Use adaptor to plug a three-pin into a two-pin socket
    let adaptor = ThreeToTwoPinSocketAdaptor;
    let two_plug = adaptor.adapt(three_plug);
    let two_socket = TwoPinSocket;
    println!(
        "Two socket accepts adapted plug? {}",
        two_socket.accept(&two_plug)
    );
}
