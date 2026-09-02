package Exercise7;

import java.util.Comparator;

public class NodeCostComparator implements Comparator<Node<?,?>> {

    @Override
    public int compare(Node<?, ?> o1, Node<?, ?> o2) {
        return Double.compare(o1.cost, o2.cost);
    }
    
}
