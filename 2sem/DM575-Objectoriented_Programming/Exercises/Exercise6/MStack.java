package Exercise6;

import java.util.Iterator;
import java.util.NoSuchElementException;

import Exercise6.SinglyLinkedList.InnerSinglyLinkedList;

public class MStack<E> implements MList<E> {

    public static void main(String[] args) {
        
    }

    private class Node {
        E data;
        Node next;
        Node prev;

        Node(E data) {
            this.data = data;
            this.prev = null;
            this.next = null;
        }
    }

    Node head;

    private int size = 0;

	@Override
	public boolean contains(Object o) {
        for (int i = 0; iterator().hasNext(); i++) {
            if (get(i) == o) {
                return true;
            }
        }
        return false;
	}

	@Override
	public void clear() {
		this.head = null;
        this.size = 0;
	}

	@Override
    public MListIterator<E> iterator() {
        return new InnerMStack();
    }

    public class InnerMStack implements MListIterator<E> {
        private Node current;
        private int index;

        InnerMStack() {
            this.current = MStack.this.head;
            this.index = 0;
            //this.size = 0;
        }

        void push(E e) {
            add(e);
            size++;
        }
        E peek() {
            if (size == 0) {
                throw new NoSuchElementException("Stack is empty");
            }
            return get(size - 1);
        }

        E pop() {
            E element = get(size - 1);
            MStack.this.remove(element);
            return element;
        }

        @Override
        public boolean hasNext() {
            return current.next != null;
        }

        @Override
        public E next() {
            index++;
            return this.current.next.data;
        }

        @Override
        public int nextIndex() {
            return index + 1;
        }
    }

	@Override
	public boolean isEmpty() {
		if (this.size == 0) {
            return true;
        } else {
           return false; 
        }	
	}

	@Override
	public E get(int index) {
		// Implementation here
		return null;
	}

	@Override
	public boolean remove(Object o) {
		// Implementation here
		return false;
	}

	@Override
	public boolean add(E e) {
		// Implementation here
		return false;
	}

	@Override
	public int size() {
		// Implementation here
		return 0;
	}
}
