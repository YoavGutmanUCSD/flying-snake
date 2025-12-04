#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Check<const N: usize>(pub [bool; N]);

impl<const N: usize> Check<N> {
    pub fn all_true() -> Self {
        Self([true; N])
    }

    pub fn is_enabled(&self, idx: usize) -> bool {
        self.0.get(idx).copied().unwrap_or(false)
    }
}

impl<const N: usize> Default for Check<N> {
    fn default() -> Self {
        Self::all_true()
    }
}
