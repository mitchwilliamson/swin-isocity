unit gInput;
//=============================================================================
// gInput.pas
//=============================================================================
//
// Responsible for handling user input, selecting objects, etc.
//
//=============================================================================

interface
	uses SwinGame, sgTypes, gTypes, gBuilding, gMap, gUnitAi, gUnit, sysUtils;
	procedure HandleInput(var game : GameData);

implementation
	// Move the camera using mouse and keyboard
	procedure ControlCamera();
	begin
		if (KeyDown(DownKey) or KeyDown(SKey) or (MouseY() > ScreenHeight() - MOUSE_MOVE_PADDING)) and (CameraY() + CAMERA_SPEED < -170) then
		    MoveCameraBy(0 ,+CAMERA_SPEED );
		if (KeyDown(UpKey) or KeyDown(WKey) or (MouseY() < MOUSE_MOVE_PADDING)) and (CameraY() - CAMERA_SPEED > -520) then
		    MoveCameraBy(0 ,-CAMERA_SPEED );
		if ((KeyDown(LeftKey)) or KeyDown(AKey) or (MouseX() < MOUSE_MOVE_PADDING)) and (CameraX() - CAMERA_SPEED > 0) then
		    MoveCameraBy(-CAMERA_SPEED ,0 );
		if (KeyDown(RightKey) or KeyDown(DKey) or (MouseX() > ScreenWidth() - MOUSE_MOVE_PADDING)) and (CameraX() + CAMERA_SPEED < 830) then
		    MoveCameraBy(+CAMERA_SPEED ,0 );

	end;

	// When a user clicks, select tiles, vehicles, etc.
	procedure HandleClick(var game : GameData);
	var
		point, realMouse: Point2D;
		i: Integer;
		spr: Sprite;
		skip: Boolean;
		neighbours: TileArray;
	begin
		skip := false;
		// Delect entities
		if MouseClicked(RightButton) then
		begin
			for i := 0 to High(game.units) do
			begin
				game.units[i].selected := false;
			end;
			game.mouseLoc.mouseStatus := MOUSE_NORMAL;
			game.mouseLoc.hasSelected := false;
		end;

		if MouseClicked(LeftButton) then
		begin
			// UI Zone selector
			if (((MouseX() < 320) or (MouseX() > SCREEN_WIDTH-64)) and (MouseY() > 720 - 64)) then
			begin
				PlaySoundEffect('UIClick');
				if MouseX() < 64 then
					game.zoningType := TERRAIN_ZONE
				else if ((MouseX() > 64) and (MouseX() < 128)) then
					game.zoningType := TERRAIN_COMZONE
				else if ((MouseX() > 128) and (MouseX() < 192)) then
					game.zoningType := TERRAIN_ROAD						
				else if ((MouseX() > 192) and (MouseX() < 256)) then
					game.zoningType := TERRAIN_POWER						
				else if ((MouseX() > 256) and (MouseX() < 320)) then
					game.zoningType := TERRAIN_PLACEWATER				
				else if ((MouseX() > SCREEN_WIDTH-64)) then
					game.zoningType := TERRAIN_REMOVE;

				exit;
			end;
		end;

		if MouseDown(LeftButton) then
		begin
			point := ScreenToWorld(MousePosition());

			// Bulldoze
			if game.zoningType = TERRAIN_REMOVE then
			begin
				game.map[Round(point.x),Round(point.y)].tType := game.tileTypes[1];
				game.map[Round(point.x),Round(point.y)].bitmap := BitmapNamed('TileGrass01');
				game.map[Round(point.x),Round(point.y)].terrainType := TERRAIN_NORMAL;
				exit;
			end;

			// Spawn Power Plant
			if ((game.zoningType = TERRAIN_POWER) and (PlotIsAvailable(game,point))) then
			begin
				game.map[Round(point.x),Round(point.y)].tType := game.tileTypes[1];
				game.map[Round(point.x),Round(point.y)].bitmap := BitmapNamed('TileGrass01');
				game.map[Round(point.x),Round(point.y)].terrainType := TERRAIN_POWER;
				SpawnBuilding(game, point, TERRAIN_POWER);
				exit;
			end;

			// Spawn Water Pump
			if ((game.zoningType = TERRAIN_PLACEWATER) and (PlotIsAvailable(game,point))) then
			begin
				game.map[Round(point.x),Round(point.y)].tType := game.tileTypes[1];
				game.map[Round(point.x),Round(point.y)].bitmap := BitmapNamed('TileGrass01');
				game.map[Round(point.x),Round(point.y)].terrainType := TERRAIN_POWER;
				SpawnBuilding(game, point, TERRAIN_PLACEWATER);
				exit;
			end;

			if game.map[Round(point.x),Round(point.y)].tType.terrainType = TERRAIN_NORMAL then
			begin

				// Zone Road
				if game.zoningType = TERRAIN_ROAD then
				begin
					neighbours := FindNeighbours(game,point);
					game.map[Round(point.x),Round(point.y)].bitmap := BitmapNamed('TileRoad01');
					for i := 0 to High(neighbours) do
					begin
						if ((neighbours[i].terrainType = TERRAIN_ROAD) and (i = 1)) or ((neighbours[i].terrainType = TERRAIN_ROAD) and (i = 2)) then
						begin
							game.map[Round(point.x),Round(point.y)].bitmap := BitmapNamed('TileRoad02');
						end;
					end;

					game.map[Round(point.x),Round(point.y)].tType := game.tileTypes[3];
					game.map[Round(point.x),Round(point.y)].terrainType := game.zoningType;
				end
				else // Zone Residential/Commercial 
				begin
					game.map[Round(point.x),Round(point.y)].tType := game.tileTypes[5];
					game.map[Round(point.x),Round(point.y)].bitmap := BitmapNamed('TileGrassZoned');
					game.map[Round(point.x),Round(point.y)].terrainType := game.zoningType;
					SpawnUnit(game, game.unitTypes[0], PointOfInt(165,-81));
					game.units[High(game.units)].destination := point;
					FindPath(game, game.units[High(game.units)]);
				end;
			end;

			// Select Buildings
			for i := 0 to High(game.buildings) do
			begin
				spr := game.buildings[i].sprite;
				realMouse := ScreenToReal(MousePosition());
				if (SpriteX(spr) < realMouse.x) and (SpriteX(spr) + SpriteWidth(spr) > realMouse.x) and (SpriteY(spr) < realMouse.y) and (SpriteY(spr) + SpriteHeight(spr) > realMouse.y) and not skip then
				begin
					game.buildings[i].selected := true;
					skip := true;	
				end
				else
				begin
					game.buildings[i].selected := false;
				end;

				if (SpriteX(spr) < realMouse.x) and (SpriteX(spr) + SpriteWidth(spr) > realMouse.x) and (SpriteY(spr) < realMouse.y) and (SpriteY(spr) + SpriteHeight(spr) > realMouse.y) then
					game.buildings[i].mouseOver := true
				else
					game.buildings[i].mouseOver := false;
			end;
		end;
	end;

	// Mouse over buildings
	procedure HandleMouseover(var game : GameData);
	var
		i: Integer;
		spr: Sprite;
		realMouse: Point2D;
	begin
		for i := 0 to High(game.buildings) do
		begin
			spr := game.buildings[i].sprite;
			realMouse := ScreenToReal(MousePosition());

			game.buildings[i].mouseOver := false;

			if (SpriteX(spr) < realMouse.x) and (SpriteX(spr) + SpriteWidth(spr) > realMouse.x) and (SpriteY(spr) < realMouse.y) and (SpriteY(spr) + SpriteHeight(spr) > realMouse.y) then
			begin
				game.buildings[i].mouseOver := true;
				// exit (); // make sure we can only mouse over one building
			end;
		end;
	end;

	// Calls other input procedures
	procedure HandleInput(var game : GameData);
	begin
		ControlCamera();
		HandleClick(game);
		HandleMouseover(game);
	end;

end.
