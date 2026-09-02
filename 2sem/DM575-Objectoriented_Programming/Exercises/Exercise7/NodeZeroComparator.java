package Exercise7;

import java.util.Comparator;

public class NodeZeroComparator implements Comparator<Node<?,?>> {

    @Override
    public int compare(Node<?, ?> o1, Node<?, ?> o2) {
        return 0;
    }
    
}
