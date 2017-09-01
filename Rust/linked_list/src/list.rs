use std::rc::Rc;

#[derive(Debug)]
struct ListNode<T> {
    val: T,
    next: Option<Rc<ListNode<T>>>
}

#[derive(Debug)]
pub struct List<T> {
    head: Option<Rc<ListNode<T>>>
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn insert_front(&self, val: T) -> Self {
        List { head: Some( Rc::new( ListNode {
            val: val,
            next: self.head.clone()
        } ) ) }
    }

    pub fn tail(&self) -> Self {
        List { head: self.head.as_ref().and_then( |head| { head.next.clone() } ) }
    }

    pub fn front(&self) -> Option<&T> {
        self.head.as_ref().map( |head| { &head.val } )
    }

    pub fn iter(&self) -> Iter<T> {
        Iter { next: self.head.as_ref().map( |head| { &**head } ) }
    }
}

pub struct Iter<'a, T: 'a> {
    next: Option<&'a ListNode<T>>
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.map( |node| {
            self.next = node.next.as_ref().map( |next| { &**next } );
            &node.val
        } )
    }
}

mod test {
    use super::List;

    #[test]
    fn test_list() {
        let list = List::<i32>::new();
        assert_eq!(list.front(), None);

        let list = list.insert_front(1).insert_front(2).insert_front(3);
        assert_eq!(list.front(), Some(&3));

        let list = list.tail();
        assert_eq!(list.front(), Some(&2));

        let list = list.tail();
        assert_eq!(list.front(), Some(&1));

        let list = list.tail();
        assert_eq!(list.front(), None);
    }

    #[test]
    fn test_list_iter() {
        let list = List::<i32>::new();
        let list = list.insert_front(1).insert_front(2).insert_front(3);
        let mut iter = list.iter();

        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(list.front(), Some(&3));

    }
}
