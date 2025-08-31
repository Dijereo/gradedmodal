use crate::{frame::FrameCondition, model::NodePosition, tableau2::LabeledFormula};

pub(crate) struct GraphInner {
    pub(crate) adjlist: Vec<(NodeInner, Vec<EdgeInner>)>,
    pub(crate) frameconds: FrameCondition,
}

pub(crate) struct NodeInner {
    pub(crate) count: usize,
    pub(crate) formulae: Vec<LabeledFormula>,
    pub(crate) position: NodePosition,
}

pub(crate) struct EdgeInner {
    pub(crate) target: usize,
    pub(crate) sym: bool,
}
