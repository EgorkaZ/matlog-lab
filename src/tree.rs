#![allow(dead_code)]
use std::ops::Deref;

pub trait Tree
{
    type Child: Deref<Target = Self> + Sized;

    fn children(&self) -> ChildrenIter<'_, Self>;

    fn dfs<'a>(start: &'a Self::Child) -> DfsIterator<'a, Self>
        where Self: 'a
    {
        DfsIterator::begin_in(start)
    }
}

pub struct ChildrenIter<'a, T: Tree + ?Sized>
{
    children: Vec<&'a T::Child>,
    curr: usize,
}

impl<'a, T: Tree + ?Sized> ChildrenIter<'a, T> {
    pub fn new(children: Vec<&'a T::Child>) -> Self {
        ChildrenIter{ children, curr: 0 }
    }
}

impl<'a, T: 'a + Tree + ?Sized> Iterator for ChildrenIter<'a, T>
{
    type Item = &'a T::Child;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr < self.children.len() {
            let res = self.children[self.curr];
            self.curr += 1;
            Some(res)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DfsDir {
    In,
    Out
}

pub struct DfsIterator<'a, T: Tree + ?Sized> {
    children_stack: Vec<ChildrenIter<'a, T>>,
    parent_stack: Vec<&'a T::Child>,
}

impl<'a, T: 'a + Tree + ?Sized> DfsIterator<'a, T>{
    pub fn begin_in(node: &'a T::Child) -> Self {
        DfsIterator{ children_stack: vec![], parent_stack: vec![node] }
    }
}

impl<'a, T> Iterator for DfsIterator<'a, T>
    where T: 'a + Tree + ?Sized
{
    type Item = (&'a T::Child, DfsDir);

    fn next(&mut self) -> Option<Self::Item> {
        // parent_stack either has 1 more element than children_stack or has same count of elements
        //     in first case we've just come in the node, return it as 'In' and add it's children to children_stack
        //     in second case we're processing current parent's children: push a child as new parent, go to first case
        //     if current children are all processed, return current parent as 'Out', delete from stack
        loop {
            if self.children_stack.len() < self.parent_stack.len() {
                // parent_stack.len() is at least 1
                let curr_parent = self.parent_stack.last().unwrap();
                self.children_stack.push(curr_parent.children());
                return Some((*curr_parent, DfsDir::In))
            } else {
                // equal lengths, mb 0
                if let Some(curr_children) = self.children_stack.last_mut() {
                    // neither length is 0
                    if let Some(curr_child) = curr_children.next() {
                        self.parent_stack.push(curr_child);
                    } else {
                        let res = self.parent_stack.pop().unwrap();
                        self.children_stack.pop();
                        return Some((res, DfsDir::Out));
                    }
                } else {
                    // both 0
                    return None
                }
            }
        }
    }
}
