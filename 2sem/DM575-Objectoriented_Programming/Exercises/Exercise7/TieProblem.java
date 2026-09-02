package Exercise7;

public class TieProblem extends Connector{

    @Override
    public boolean isGoal(Board state) {
        return (state.tied() && isState(state));
    }

}
