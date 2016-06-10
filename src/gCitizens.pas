unit gCitizens;
//=============================================================================
// gCitizens.pas
//=============================================================================
//
// Responsible for managing citizens (creating, looking for work, death, etc)
//
//=============================================================================

interface
	uses SwinGame, sgTypes, gTypes, gMap;

	procedure CreateCitizens(var game : GameData; home : Point2D; count : Integer);

implementation

	function RandomName(var game : GameData) : String;
	var
		i: Integer;
	begin
		i := Random(High(game.names));
		result := game.names[i];
	end;

	procedure CreateCitizen(var game : GameData; home : Point2D);
	var
		idx: Integer;
	begin
		SetLength(game.citizens, Length(game.citizens) + 1);
		idx := High(game.citizens);
		game.citizens[idx].home := home;
		game.citizens[idx].firstName := RandomName(game);
		game.citizens[idx].lastName := RandomName(game);
	end;

	procedure CreateCitizens(var game : GameData; home : Point2D; count : Integer);
	var
		i: Integer;
	begin
		for i := 1 to count do
		begin
			CreateCitizen(game, home);
		end;
	end;

end.
