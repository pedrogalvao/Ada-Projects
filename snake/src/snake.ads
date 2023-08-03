package snake is

   type Cell is (Empty, Snake_Up, Snake_Down, Snake_Left, Snake_Right, Fruit);
   type Grid is array(Natural range <>, Natural range <>) of Cell;
   type Position is array(1..2) of Natural;
   type Direction is (Up, Down, Left, Right);
   
   type State(Size : Positive) is record
      state_grid : Grid(1..Size, 1..Size);
      snake_head : Position;
      snake_tail : Position;
      snake_direction : Direction;
      game_over : Boolean := False;
   end record;

   procedure Next_State(s: in out State);
   procedure Print_State(s: in State);
   function Create_State(size: Natural) return State;
   procedure Clear_Screen;
   function Is_Snake(c: Cell) return Boolean;
end snake;
