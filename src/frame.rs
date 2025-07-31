#[derive(Clone, Copy, Debug)]
pub(crate) enum FrameCondition {
    K,
    D,
    // T,
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
    KB5,
    S5,
}

impl FrameCondition {
    pub(crate) const fn serial(&self) -> bool {
        match self {
            FrameCondition::K | FrameCondition::K45 | FrameCondition::KB5 => false,
            FrameCondition::D | FrameCondition::D45 | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn reflexive(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5 => false,
            FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn spotlit(&self) -> bool {
        match self {
            FrameCondition::K
            | FrameCondition::D
            | FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => false,
        }
    }

    pub(crate) const fn luminal(&self) -> bool {
        match self {
            FrameCondition::K | FrameCondition::D => false,
            FrameCondition::K45
            | FrameCondition::D45
            | FrameCondition::KB5
            | FrameCondition::S5 => true,
        }
    }

    pub(crate) const fn cliqued(&self) -> bool {
        match self {
            FrameCondition::K | FrameCondition::D | FrameCondition::K45 | FrameCondition::D45 => {
                false
            }
            FrameCondition::KB5 | FrameCondition::S5 => true,
        }
    }
}
