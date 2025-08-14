use crate::{rules3::Feasibility, transit::{Constraints, ParallelWorlds}};

pub(crate) struct TransitB {
    pub(crate) feasibility: Feasibility,
    pub(crate) paraws: ParallelWorlds<Self>,
    pub(crate) constraints: Constraints,
    pub(crate) solution: Vec<u32>,
}
