package Exercise7;

public class PlayerVictoryProblem extends Connector {
    
    public Player player;

    @Override
    public boolean isGoal(Board state) {
        return (state.won() && isState(state) && !state.nextPlayer().equals(player));
    }
}
