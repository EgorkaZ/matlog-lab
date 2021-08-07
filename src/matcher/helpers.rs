use std::{cmp::Eq, collections::{HashMap}, hash::Hash, rc::Rc};

pub trait SubstContainer<Key: Clone, Type>
{
    fn get_subst(&self, key: &Key) -> Option<&'_ Rc<Type>>;
    fn substitute(&mut self, key: &Key, subst: Rc<Type>);

    fn check_substitution(&mut self, key: &Key, subst: &Rc<Type>) -> Result<(), Rc<Type>>
    {
        match self.get_subst(&key) {
            Some(found) => {
                if Rc::ptr_eq(subst, found) {
                    Ok(())
                } else {
                    Err(Rc::clone(found))
                }
            },
            None => {
                self.substitute(key, Rc::clone(subst));
                Ok(())
            }
        }
    }
}

impl<Key, Type> SubstContainer<Key, Type> for HashMap<Key, Rc<Type>>
    where Key: Clone + Eq + Hash
{
    fn get_subst(&self, key: &Key) -> Option<&'_ Rc<Type>> {
        self.get(&key)
    }

    fn substitute(&mut self, key: &Key, subst: Rc<Type>) {
        self.insert(key.clone(), subst);
    }
}
