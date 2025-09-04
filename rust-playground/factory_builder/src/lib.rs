//! factory_builder: example crate showing IFactory<T> and IBuilder<T> patterns in Rust.
#![deny(missing_docs)]

/// Generic factory trait producing T
pub trait IFactory<T> {
    /// Create a new instance of T
    fn create(&self) -> T;
}

/// Generic builder trait producing T
pub trait IBuilder<T> {
    /// Build and return T
    fn build(self) -> T;
}

/// Product type: Vehicle enum for the factory example
#[derive(Debug, PartialEq, Eq)]
pub enum Vehicle {
    /// A small motorcycle with engine displacement in cc
    Motorcycle {
        /// engine displacement in cubic centimeters
        cc: u32,
    },
    /// A car with a number of seats
    Car {
        /// number of seats in the car
        seats: u8,
    },
    /// A truck with capacity in tons
    Truck {
        /// payload capacity in tons
        capacity_tons: u32,
    },
}

/// A simple vehicle factory that holds the kind to create
pub struct VehicleFactory {
    kind: String,
}

impl VehicleFactory {
    /// Create a new factory configured with a `kind` string
    pub fn new(kind: &str) -> Self {
        Self {
            kind: kind.to_string(),
        }
    }
}

impl IFactory<Vehicle> for VehicleFactory {
    fn create(&self) -> Vehicle {
        match self.kind.as_str() {
            "motorcycle" => Vehicle::Motorcycle { cc: 125 },
            "car" => Vehicle::Car { seats: 5 },
            "truck" => Vehicle::Truck { capacity_tons: 10 },
            _ => Vehicle::Car { seats: 4 },
        }
    }
}

/// Product for the builder example: House
#[derive(Debug, PartialEq, Eq)]
/// A simple house product constructed by the builder
pub struct House {
    /// Number of windows
    pub windows: u8,
    /// Number of doors
    pub doors: u8,
    /// Whether the house has a garage
    pub garage: bool,
}

/// Concrete builder for House which implements IBuilder<House>
#[derive(Debug, Default)]
pub struct HouseBuilder {
    windows: u8,
    doors: u8,
    garage: bool,
}

impl HouseBuilder {
    /// Create a new HouseBuilder
    pub fn new() -> Self {
        Self::default()
    }
    /// Set number of windows
    pub fn windows(mut self, n: u8) -> Self {
        self.windows = n;
        self
    }
    /// Set number of doors
    pub fn doors(mut self, n: u8) -> Self {
        self.doors = n;
        self
    }
    /// Set garage flag
    pub fn garage(mut self, v: bool) -> Self {
        self.garage = v;
        self
    }
}

impl IBuilder<House> for HouseBuilder {
    fn build(self) -> House {
        House {
            windows: self.windows,
            doors: self.doors,
            garage: self.garage,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn factory_creates_expected_variants() {
        let f = VehicleFactory::new("motorcycle");
        assert_eq!(f.create(), Vehicle::Motorcycle { cc: 125 });
        let f2 = VehicleFactory::new("truck");
        assert_eq!(f2.create(), Vehicle::Truck { capacity_tons: 10 });
    }

    #[test]
    fn builder_constructs_house() {
        let h = HouseBuilder::new().windows(6).doors(2).garage(true).build();
        assert_eq!(
            h,
            House {
                windows: 6,
                doors: 2,
                garage: true
            }
        );
    }
}
