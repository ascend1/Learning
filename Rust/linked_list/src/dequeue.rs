use std::rc::Rc;
use std::cell::{ Ref, RefMut, RefCell };

type Link<T> = Option<Rc<RefCell<ListNode<T>>>>;

#[derive(Debug)]
struct ListNode<T> {
    val: T,
    prev: Link<T>,
    next: Link<T>
}

impl<T> ListNode<T> {
    fn new(val: T) -> Rc<RefCell<Self>> {
        Rc::new( RefCell::new( ListNode { val: val, prev: None, next:None } ) )
    }
}

#[derive(Debug)]
pub struct Dequeue<T> {
    head: Link<T>,
    tail: Link<T>
}

impl<T> Dequeue<T> {
    pub fn new() -> Self {
        Dequeue { head: None, tail: None }
    }

    pub fn push_front(&mut self, val: T) {
        let new_node = ListNode::new(val);
        match self.head.take() {
            None => {
                self.head = Some( new_node );
                self.tail = self.head.clone();
            }
            Some( old_head ) => {
                old_head.borrow_mut().prev = Some( new_node.clone() );
                new_node.borrow_mut().next = Some( old_head );
                self.head = Some( new_node );
            }
        }
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.head.take().map( |old_head| {
            match old_head.borrow_mut().next.take() {
                Some( new_head ) => {
                    new_head.borrow_mut().prev.take();
                    self.head = Some( new_head );
                }
                None => {
                    self.tail.take();
                }
            }
            Rc::try_unwrap(old_head).ok().unwrap().into_inner().val
        } )
    }

    pub fn peek_front(&self) -> Option<Ref<T>> {
        self.head.as_ref().map( |head| {
            Ref::map( head.borrow(), |node| { &node.val } ) // this is total magic...
        } )
    }

    pub fn peek_front_mut(&self) -> Option<RefMut<T>> {
        self.head.as_ref().map( |head| {
            RefMut::map( head.borrow_mut(), |node| { &mut node.val } )
        } )
    }
}

impl<T> Drop for Dequeue<T> {
    fn drop(&mut self) {
        while self.pop_front().is_some() {}
    }
}

#[cfg(test)]
mod test {
    use super::Dequeue;

    #[test]
    fn test_dequeue() {
        let mut deq = Dequeue::<i32>::new();
        assert_eq!(deq.pop_front(), None);

        deq.push_front(1);
        deq.push_front(2);
        deq.push_front(3);

        assert_eq!(deq.pop_front(), Some(3));
        assert_eq!(deq.pop_front(), Some(2));

        deq.push_front(4);
        deq.push_front(5);

        assert_eq!(deq.pop_front(), Some(5));
        assert_eq!(deq.pop_front(), Some(4));
        assert_eq!(deq.pop_front(), Some(1));
        assert_eq!(deq.pop_front(), None);
    }

    #[test]
    fn test_dequeue_peek() {
        let mut deq = Dequeue::<i32>::new();
        assert!(deq.peek_front().is_none());

        deq.push_front(1);
        deq.push_front(2);
        deq.push_front(3);

        assert_eq!(&*deq.peek_front().unwrap(), &3);
        assert_eq!(&*deq.peek_front_mut().unwrap(), &mut 3);
    }
}
