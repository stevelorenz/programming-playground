// ! High-level biological structures

pub mod leaves;
pub mod roots;
pub mod stems;

pub use self::leaves::Leaf;
pub use self::roots::Root;

use self::roots::RootSet;
use self::stems::StemSet;

pub enum FernType {
    Fiddlehead,
}

pub struct Fern {
    pub roots: RootSet,
    pub stems: StemSet,
}

impl Fern {
    // The fancy constructor
    pub fn new(_type: FernType) -> Fern {
        return Fern {
            roots: vec![],
            stems: vec![stems::Stem { furled: true }],
        };
    }

    pub fn is_fully_unfurled(&self) -> bool {
        // The iterator accepts methods like all, map.
        // Then a lambda can be passed to the function.
        return self.stems.iter().all(|s| !s.furled);
    }

    pub fn is_furled(&self) -> bool {
        return !self.is_fully_unfurled();
    }
}

#[doc(alias = "route")]
pub struct VascularPath {
    pub from: bool,
    pub to: bool,
}

/// Create and returh a [`VascularPath`] which represents the path from The
/// given [`Root`][r] to the given [`Leaf`]
///
/// [r]: roots::Root
pub fn trace_path(leaf: &leaves::Leaf, root: &roots::Root) -> VascularPath {
    return VascularPath {
        from: leaf.x,
        to: root.x,
    };
}
