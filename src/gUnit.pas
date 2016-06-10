unit gUnit;

interface
	uses SwinGame, sgTypes, sysUtils, gMap, gTypes;
	procedure SpawnUnit(var game : GameData; unitType : UnitType; loc : Point2D);
	procedure RemoveUnit(var game : GameData; idx : Integer);

implementation
	procedure SpawnUnit(var game : GameData; unitType : UnitType; loc : Point2D);
	var
		idx: Integer;
		// layers : BitmapArray;
		// layerNames : StringArray;
	begin
		SetLength(game.units, Length(game.units) + 1);
		idx := High(game.units);

		game.units[idx].sprite := CreateSprite('Tank', BitmapNamed('AgentTruckRedTR'));
		game.units[idx].selected := false;
		game.units[idx].loc := Point(loc.x,loc.y);
		game.units[idx].hp := 100;
		game.units[idx].moveSpeed := 1;
		SpriteSetX(game.units[idx].sprite, loc.x);
		SpriteSetY(game.units[idx].sprite, loc.y);
		game.units[idx].movingTo := Point(loc.x,loc.y);		
		game.units[idx].destination := Point(5,5);		
		game.units[idx].toDelete := false;

		SpriteAddLayer(game.units[idx].sprite, BitmapNamed('AgentTruckRedTL'), 'TopLeft');
		SpriteAddLayer(game.units[idx].sprite, BitmapNamed('AgentTruckRedTR'), 'TopRight');
		SpriteAddLayer(game.units[idx].sprite, BitmapNamed('AgentTruckRedBL'), 'BottomLeft');
		SpriteAddLayer(game.units[idx].sprite, BitmapNamed('AgentTruckRedBR'), 'BottomRight');
		// game.units[idx].ai.status := INIT;
		// game.units[idx].unitType := unitType;
		// game.units[idx].ai.timer := CreateTimer();
		// 
		DebugMsg('Agent: New Agent Spawned at ' + PointToString(loc)); 
	end;

	procedure RemoveUnit(var game : GameData; idx : Integer);
	var
		i: Integer;
	begin
		DebugMsg('Agent: Agent of index '+IntToStr(idx)+' despawning...');
		if Length(game.units) = 1 then
		begin
			SetLength(game.units, 0);
			DebugMsg('Agent: Agent of index '+IntToStr(idx)+' despawned.');
			exit ();
		end;

		for i := idx + 1 to High(game.units) do
		begin
			game.units[i - 1] := game.units[i];
		end;
		SetLength(game.units, Length(game.units) - 1);
		DebugMsg('Agent: Agent of index '+IntToStr(idx)+' despawned.');

	end;
end.
