#[derive(Clone, Copy, Debug)]
pub(crate) enum FrameCondition {
    K,
    D,
    T,
    // KB,
    // DB,
    // TB,
    // K4,
    // D4,
    // S4,
    // K5,
    // D5,
    K45,
    D45,
    // KB45,
    // S5,
}

impl FrameCondition {
    pub(crate) fn serial(&self) -> bool {
        match self {
            FrameCondition::K | FrameCondition::K45 => false,
            FrameCondition::D | FrameCondition::T | FrameCondition::D45 => true,
        }
    }
    
    pub(crate) fn reflexive(&self) -> bool {
        match self {
            FrameCondition::K | FrameCondition::D | FrameCondition::K45 | FrameCondition::D45 => {
                false
            }
            FrameCondition::T => true,
        }
    }

    pub(crate) fn luminal(&self) -> bool {
        match self {
            FrameCondition::K | FrameCondition::D | FrameCondition::T => false,
            FrameCondition::K45 | FrameCondition::D45 => true,
        }
    }
}
