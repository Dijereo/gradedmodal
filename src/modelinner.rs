use crate::{model::NodePosition, tableau2::LabeledFormula};

pub(crate) struct GraphInner {
    pub(crate) adjlist: Vec<(NodeInner, Vec<usize>)>,
}

pub(crate) struct NodeInner {
    pub(crate) count: usize,
    pub(crate) formulae: Vec<LabeledFormula>,
    pub(crate) position: NodePosition,
}
