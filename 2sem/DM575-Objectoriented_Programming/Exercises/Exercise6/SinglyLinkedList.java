package Exercise6;

import java.util.Iterator;

public class SinglyLinkedList<E> implements MList<E> {

    public static void main(String[] args) {
        
    }

    private class Node {
        E data;
        Node next;

        Node(E data) {
            this.data = data;
            this.next = null;
        }
    }

    Node head;

    private int size = 0;

    @Override
    public boolean add(E e) {
        if (head == null) {
            head = new Node(e);
            size++;
            return true;
        } else {
            Node current = head;
            while (current.next != null) {
                current = current.next;
            }
            current.next = new Node(e);
            size++;
            return true;
        } 
    }

    @Override
    public E get(int index) {
        Node current = head.next;
        for (int i = 0; i < index; i++) {
            current = current.next;
        }
        return current.data;
    }

    @Override
    public MListIterator<E> iterator() {
        return new InnerSinglyLinkedList();
    }

    public class InnerSinglyLinkedList implements MListIterator<E> {
        private Node current;
        private int index;
        //private int size;

        InnerSinglyLinkedList() {
            this.current = SinglyLinkedList.this.head;
            this.index = 0;
            //this.size = 0;
        }
    
        @Override
        public boolean hasNext() {
            return this.current != null;
        }

        @Override
        public E next() {
            if (current == null) {
                throw new IllegalStateException("No more elements");
            }
            E data = this.current.data;
            this.current = this.current.next;
            index++;
            return data;
        }

        @Override
        public int nextIndex() {
            return index + 1;
        }
    }

    @Override
    public void clear() {
        this.head = null;
        this.size = 0;
    }

    @Override
    public boolean contains(Object o) {
        for (int i = 0; iterator().hasNext(); i++) {
            if (get(i).equals(o)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean remove(Object o) {
        if (this.size != 0) {
            if (this.size == 1) {
                head = null;
                return true;
            }
            Node current = head;
            Node prevNode = null;
            while (current != null) {
                if (current.data.equals(o)) {
                    if (prevNode == null) {
                        head = current.next;
                    } else {
                        prevNode.next = current.next;
                    }
                    size--;
                    return true;
                }
                prevNode = current;
                current = current.next;
            }

        }
        return false;
    }

    @Override
    public int size() {
        return this.size;
    }

    @Override
    public boolean isEmpty() {
        if (head == null) {
            return false;
        } else {
            return true;
        }
    }
}