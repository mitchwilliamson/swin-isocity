unit gUnitAI;
//=============================================================================
// gUnitAI.pas
//=============================================================================
//
// Responsible for managing agent AI
//
//=============================================================================

interface
	uses SwinGame, gMap, sgTypes, gTypes, sysUtils, gRender, gUnit, gBuilding;

	/// Handles the tick-based AI of agents
	/// 
	/// @param game The game data
	procedure AITick(var game : GameData);
	function FindPath(var game : GameData; var agent : UnitEntity): Integer;

implementation
	// Update entity location values with sprite location
	procedure UpdateEntity(var u : UnitEntity);
	begin
		u.loc.x := SpriteX(u.sprite);
		u.loc.y := SpriteY(u.sprite);
	end;

	// Check whether the point 1 is close to point 2
	function WithinRange(p1, p2 : Point2D): Boolean;
	begin
		if  (p1.x > p2.x - 20) 
		and (p1.y > p2.y - 20) 
		and (p1.x < p2.x + 20) 
		and (p1.y < p2.y + 20) then
		begin
			exit (true);
		end;
		exit (false);
	end;

	// Remove node from nodearray
	procedure RemoveArrayIndex(var arr : NodeArray; idx : Integer);
	var
		i: Integer;
	begin
		for i := idx to High(arr) do
		begin
			arr[i] := arr[i + 1];
		end;
		SetLength(arr, Length(arr) - 1);
	end;

	// Check if a node exists in a nodearray
	function ClosedSetExists(closedSet : NodeArray; tile : Tile): Boolean;
	var
		i: Integer;
	begin
		for i := 0 to High(closedSet) do
		begin
			if VectorsEqual(closedSet[i].loc, tile.loc) then
			begin
				// WriteLn(PointToString(closedSet[i].loc) + ' exists!');
				exit (true);
			end;
		end;
		exit (false);
	end;

	// Check if a node exists in a nodearray
	function ClosedSetExists(closedSet : NodeArray; node : Node): Boolean;
	var
		i: Integer;
	begin
		for i := 0 to High(closedSet) do
		begin
			if VectorsEqual(closedSet[i].loc, node.loc) then
			begin
				// WriteLn(PointToString(closedSet[i].loc) + ' exists!');
				exit (true);
			end;
		end;
		exit (false);
	end;


	// Search for a node in a nodearray and return its index
	function SearchSet(theSet : NodeArray; node : Node): Integer;
	var
		i: Integer;
	begin
		for i := 0 to High(theSet) do
		begin
			if VectorsEqual(theSet[i].loc, node.loc) then
			begin
				// WriteLn(PointToString(closedSet[i].loc) + ' exists!');
				exit (i);
			end;
		end;
		exit (-1);
	end;

	// Use the A* algorithm to find a path from agent.loc to agent.destination
	function FindPath(var game : GameData; var agent : UnitEntity): Integer;
	var
		openSet, closedSet, temp, path: NodeArray;
		tempNode, lowest : Node;
		start, goal : Node;
		gScore, fScore, hScore, lowestVal : Double;
		targetFound : Boolean;
		i, j, lowestIdx, idx: Integer;
		tempPt : Point2D;
		neighbours : TileArray;
		tentative_gScore : Double;
	begin
		tempNode.loc := RealToWorld(CenterPoint(agent.sprite));
		start := tempNode;

		SetLength(openSet, 0);
		SetLength(closedSet, 0);

		goal.loc := agent.destination;
		// goal := tempNode;

		targetFound := false;

		SetLength(openSet, 1);

		DebugMsg('AI: Entering Pathfinding for [START: ' + PointToString(RealToWorld(CenterPoint(agent.sprite))) + '] - [END: ' + PointToString(goal.loc) + ']');

		openSet[0] := start;
		openSet[0].gScore := 0;
		openSet[0].hScore := MapDistance(openSet[0].loc, agent.destination);
		openSet[0].fCost := openSet[0].hScore;
		openSet[0].parent := -1;

		repeat
			lowestVal := 9999999999;
			for i := 0 to High(openSet) do
			begin
				// WriteLn(PointToString(openSet[i].loc));
				if openSet[i].fCost < lowestVal then
				begin
					lowest := openSet[i];
					lowestVal := openSet[i].fCost;
					lowestIdx := i;
				end;
			end;

			SetLength(closedSet, Length(closedSet) + 1);
			closedSet[High(closedSet)] := lowest;
			RemoveArrayIndex(openSet, lowestIdx);
			lowestIdx := High(closedSet);

			neighbours := FindNeighbours(game, lowest.loc);
			for i := 0 to High(neighbours) do
			begin

				FillRectangle(ColorPink, WorldToScreen(neighbours[i].loc).x, WorldToScreen(neighbours[i].loc).y, 5, 5);
				// RefreshScreen();
				if (((game.map[Round(neighbours[i].loc.x), Round(neighbours[i].loc.y)].tType.terrainType <> TERRAIN_ROAD) and not (VectorsEqual(neighbours[i].loc, goal.loc))) or ClosedSetExists(closedSet, neighbours[i])) then
					continue;

				tentative_gScore := MapDistance(start.loc, neighbours[i].loc);

				if not ClosedSetExists(openSet, neighbours[i]) then
				begin
					SetLength(openSet, Length(openSet) + 1);
					tempNode.loc := neighbours[i].loc;
					// WriteLn('asdf '+IntToStr());
					openSet[High(openSet)] := tempNode;
					openSet[High(openSet)].parent := lowestIdx;
					openSet[High(openSet)].gScore := 10;
					openSet[High(openSet)].hScore := MapDistance(openSet[High(openSet)].loc, agent.destination);
					openSet[High(openSet)].fCost := openSet[High(openSet)].gScore + openSet[High(openSet)].hScore;
				end
				else if (tentative_gScore >= openSet[High(openSet)].gScore) then
				begin
					openSet[High(openSet)].parent := lowestIdx;
				end;

				// DebugMsg('AI: No Path Found.');
				// SetLength(openSet, Length(openSet) + 1);
				// openSet[High(openSet)] := tempNode;
				// 
				// Delay(500);
			end;

			if ClosedSetExists(closedSet, goal) then
			begin
				tempNode := closedSet[SearchSet(closedSet, goal)];
				SetLength(path, 1);
				path[0] := goal;
				path[0].loc := AddVectors(goal.loc, Point(0,1));
				idx := tempNode.parent;
				while idx > 0 do
				begin
					SetLength(path, Length(path) + 1);
					path[High(path)] := closedSet[idx];
					path[High(path)].loc := AddVectors(path[High(path)].loc, Point(0,1));
					idx := closedSet[idx].parent;
				end;
				DebugMsg('AI: Path found successfully.');
				break;
			end;

		until (Length(openSet) = 0) or (Length(openSet) > 2048) or (Length(path) > 0);

		if Length(path) < 2 then 
		begin
			agent.toDelete := true;
			DebugMsg('AI: Path not found!');
		end;

		agent.path := path;
	end;

	// Handle the tick-based decisions of the AI
	procedure AITick(var game : GameData);
	var
		i, j, idx: Integer;
		tDist : Double;
		currMapTile, pos, old, temp : Point2D;
		neighbours : Array [0..7] of Tile;
		highest, active : Integer;
		distance : Single;
		diff : Vector;
		delQueue : DeleteQueue;
		doDelete : Boolean;
	begin
		for i := 0 to High(game.buildings) do 
		begin
			if (Random(1000) > 995) then
			begin
				SpawnUnit(game, game.unitTypes[0], WorldToScreen(game.buildings[i].loc));
				game.units[High(game.units)].destination := game.buildings[Random(High(game.buildings))].loc;
				FindPath(game, game.units[High(game.units)]);
			end;
		end;

		if (Random(1000) < 1) and (Length(game.buildings) > 0) then
		begin
			SpawnUnit(game, game.unitTypes[0], PointOfInt(165,-81));
			game.units[High(game.units)].destination := game.buildings[Random(High(game.buildings))].loc;
			FindPath(game, game.units[High(game.units)]);
		end;



		for i := 0 to High(game.units) do
		begin
			if (Length(game.units[i].path) <> 0) and (WithinRange(game.units[i].movingTo, SpritePosition(game.units[i].sprite))) then
			begin
				if (Length(game.units[i].path) > 0) then
				begin
					old := AddVectors(RealToWorld(game.units[i].movingTo), PointOfInt(0,1));
					old.x := Round(old.x);
					old.y := Round(old.y);
					temp := game.units[i].path[High(game.units[i].path)].loc;
					temp.x := Round(temp.x);
					temp.y := Round(temp.y);

					game.units[i].movingTo := WorldToScreen(game.units[i].path[High(game.units[i].path)].loc);
					RemoveArrayIndex(game.units[i].path, High(game.units[i].path));

					if (temp.x > old.x) and (temp.y = old.y) then
					begin
						DebugMsg('AI: Moving TopRight?');
						SpriteShowLayer(game.units[i].sprite, 'TopRight');
						active := SpriteLayerIndex(game.units[i].sprite,'TopRight');
					end;

					if (temp.x < old.x) and (temp.y = old.y) then
					begin
						DebugMsg('AI: Moving TopLeft?');
						SpriteShowLayer(game.units[i].sprite, 'TopLeft');
						active := SpriteLayerIndex(game.units[i].sprite,'TopLeft');
					end;

					if (temp.x = old.x) and (temp.y > old.y) then
					begin
						DebugMsg('AI: Moving BottomRight?');
						SpriteShowLayer(game.units[i].sprite, 'BottomRight');
						active := SpriteLayerIndex(game.units[i].sprite,'BottomRight');
					end;

					if (temp.x = old.x) and (temp.y < old.y) then
					begin
						DebugMsg('AI: Moving BottomLeft?');
						SpriteShowLayer(game.units[i].sprite, 'BottomLeft');
						active := SpriteLayerIndex(game.units[i].sprite,'BottomLeft');
					end;

					for j := 0 to SpriteLayerCount(game.units[i].sprite) - 1 do
					begin
						if active <> j then
							SpriteHideLayer(game.units[i].sprite, j);
					end;

					UpdateSprite(game.units[i].sprite);
				end;
			end
			else if (Length(game.units[i].path) = 0) and (not VectorsEqual(RealToWorld(game.units[i].loc), game.units[i].destination)) and (MapDistance(RealToWorld(game.units[i].loc), game.units[i].destination) > 2) then
			begin
				FindPath(game, game.units[i]);

				if game.units[i].toDelete then
				begin
					SetLength(delQueue, Length(delQueue) + 1);
					delQueue[High(delQueue)] := i;
				end;
			end;

			if not VectorsEqual(game.units[i].movingTo, game.units[i].loc) then
			begin
				diff := VectorFromCenterSpriteToPoint(game.units[i].sprite, game.units[i].movingTo);
				diff := VectorMultiply(diff, 0.05);
				SpriteSetVelocity(game.units[i].sprite, diff);
				MoveSprite(game.units[i].sprite);
			end;

			if WithinRange(WorldToScreen(AddVectors(game.units[i].destination, PointOfInt(0,1))), game.units[i].loc) then
			begin
				DebugMsg('Agent: Agent of index '+IntToStr(i) + ' arrived at destination of '+PointToString(game.units[i].destination));

				doDelete := true;
				for j := 0 to High(delQueue) do
				begin
					if delQueue[j] = i then
						doDelete := false;
				end;

				if doDelete then
				begin
					SetLength(delQueue, Length(delQueue) + 1);
					delQueue[High(delQueue)] := i;
				end;

				if game.map[Round(game.units[i].destination.x), Round(game.units[i].destination.y)].terrainType = TERRAIN_ZONE then
				begin
					SpawnBuilding(game, game.units[i].destination, TERRAIN_ZONE);
				end;

				if game.map[Round(game.units[i].destination.x), Round(game.units[i].destination.y)].terrainType = TERRAIN_COMZONE then
				begin
					SpawnBuilding(game, game.units[i].destination, TERRAIN_COMZONE);
				end;
				
				continue;
			end;

			// end;

	 		UpdateSprite(game.units[i].sprite);
	 		UpdateEntity(game.units[i]);
		end;

		for i := 0 to High(delQueue) do 
		begin
			RemoveUnit(game, delQueue[i]);
		end;
	end;

end.
