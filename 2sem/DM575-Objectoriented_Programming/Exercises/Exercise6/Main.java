package Exercise6;

public class Main {
    
    public static void main(String[] args) {
        MList<Integer> myList = new SinglyLinkedList<>();

        myList.add(1);
        myList.add(2);
        myList.add(3);
        myList.add(4);

        MListIterator<Integer> it = myList.iterator(); // Create the iterator after adding elements

        printList(it);
    }

    public static void printList(MListIterator<Integer> it) {
        System.out.print("[");
        if (it.hasNext()) {
            System.out.print(it.next());
        }
        while (it.hasNext()) {
            System.out.print("," + it.next());
        }
        System.out.println("]");
    }
}
