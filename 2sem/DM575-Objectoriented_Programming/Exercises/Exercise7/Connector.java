package Exercise7;

import java.util.ArrayList;
import java.util.List;

public abstract class Connector implements Problem<Board, Integer> {

    @Override
    public Board initial() {
        return new Board();
    }

    @Override
    public boolean isState(Board state) {
        return state != null;
    }

    @Override
    public boolean isGoal(Board state) {
        if (state.tied() || state.won()) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public Iterable<Integer> actions(Board state) {
        List<Integer> actions = new ArrayList<Integer>(9);
        for (int i = 0; i<= 9; i++) {
            if (state.isFree(i)) {
                actions.add(i);
            }
        }
        return actions;
    }

    @Override
    public Board result(Board state, Integer action) {
        if (isState(state)) {
            return state.play(action);
        }
        else {
            return null;
        }
    }

    @Override
    public double cost(Board state, Integer action) {
        if (isState(state) && state.isFree(action)) {
            return 1.0;
        }
        else {
            return Double.NaN;
        }
    }
    
}
