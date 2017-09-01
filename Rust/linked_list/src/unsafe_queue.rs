use std::mem;

type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    val: T,
    next: Link<T>
}

impl<T> Node<T> {
    fn new(val: T) -> Self {
        Node { val: val, next: None }
    }
}

pub struct UnsafeQueue<'a, T: 'a> {
    head: Link<T>,
    tail: Option<&'a mut Node<T>>
}

impl<'a, T> UnsafeQueue<'a, T> {
    pub fn new() -> Self {
        UnsafeQueue { head: None, tail: None }
    }

    pub fn push(&'a mut self, val: T) {
        let node = Box::new(Node::new(val));
        let new_tail = match self.tail.take() {
            Some(old_tail) => {
                old_tail.next = Some(node);
                old_tail.next.as_mut().map( |node| &mut **node )
            }
            None => {
                self.head = Some(node);
                self.head.as_mut().map( |node| &mut **node )
            }
        };
        self.tail = new_tail;
    }
}
