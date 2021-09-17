use std::{cmp::Eq, collections::{HashMap}, fmt::{Debug}, hash::Hash};

pub trait SubstContainer<Key, Substituted>
    where Key: Clone,
          Substituted: Clone,
{
    fn get_subst(&self, key: &Key) -> Option<&'_ Substituted>;
    fn substitute(&mut self, key: &Key, subst: Substituted);

    fn get_subst_sure(&self, key: &Key) -> &'_ Substituted
        where Self: Debug,
              Key: Debug,
    {
        self.get_subst(key).unwrap_or_else(|| panic!("get_subst_sure({:?}) failed. SubstContainer: {:?}", key, self))
    }

    fn check_substitution<Cmp>(&mut self, key: &Key, subst: &Substituted, eq: Cmp) -> Result<(), Substituted>
        where Cmp: FnOnce(&Substituted, &Substituted) -> bool
    {
        match self.get_subst(key) {
            Some(found) => {
                if eq(subst, found) {
                    Ok(())
                } else {
                    Err(found.clone())
                }
            },
            None => {
                self.substitute(key, subst.clone());
                Ok(())
            }
        }
    }
}

impl<Key, Substituted> SubstContainer<Key, Substituted> for HashMap<Key, Substituted>
    where Key: Clone + Eq + Hash,
          Substituted: Clone
{
    fn get_subst(&self, key: &Key) -> Option<&'_ Substituted> {
        self.get(key)
    }

    fn substitute(&mut self, key: &Key, subst: Substituted) {
        self.insert(key.clone(), subst);
    }
}
