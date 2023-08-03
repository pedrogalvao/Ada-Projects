package life is
   type Cell is (On, Off);
   type State is array(Positive range <>, Positive range <>) of Cell;
   procedure Next_State(s: in out State);
   procedure Print_State(s: in State);
   procedure Clear_Screen;
   function Create_State(size: Natural) return State;
end life;
