unit gRender;
//=============================================================================
// gRender.pas
//=============================================================================
//
// Responsible for creating the window, and drawing the map, entities, etc.
//
//=============================================================================

interface
	uses SwinGame, sgTypes, gTypes, gMap, sysUtils, gBuilding;
	procedure InitRendering();
	procedure RenderGame(var game : GameData);

implementation
	// Initial rendering routine
	procedure InitRendering();
	begin
		DebugMsg('Rendering: Initialising Rendering...');
		DebugMsg('Rendering: Starting Window - ' + APP_NAME + '(' + IntToStr(SCREEN_WIDTH) + ',' + IntToStr(SCREEN_HEIGHT)+')');
		OpenGraphicsWindow(APP_NAME, SCREEN_WIDTH, SCREEN_HEIGHT);
		ClearScreen(ColorWhite);

		if START_FULLSCREEN then
			ToggleFullscreen();

		DebugMsg('Rendering: Initialising Rendering... done.');
	end;

	// Check whether the point is within view or not
	function ShouldRender(point : Point2D): Boolean;
	begin
		result := (point.x + 256 >= CameraX()) and 
				  (point.x < CameraX() + SCREEN_WIDTH) and 
				  (point.y + 256 >= CameraY()) and 
				  (point.y < CameraY() + SCREEN_HEIGHT);
	end;

	// Render the map tiles and background
	procedure RenderMap(var game : GameData);
	var
		x,y: Integer;
		point: Point2D;
	begin
		for x := MAP_WIDTH downto 0 do
		begin
			for y := -17 to 0 do
			begin
				point.x := x + 0.5;
				point.y := y + 0.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileConcrete'), WorldToScreen(point).x, WorldToScreen(point).y);
			end;
		end;

		for y := 0 to MAP_HEIGHT do
		begin
			for x := 50 downto 32 do
			begin
				point.x := x + 0.5;
				point.y := y + 0.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileConcrete'), WorldToScreen(point).x, WorldToScreen(point).y);
			end;
		end;


		for x := MAP_WIDTH downto 0 do
		begin
			if (x mod 5 = 0) then 
			begin
				point.x := x + 0.5;
				point.y := -1.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileTunnelLeft'), WorldToScreen(point).x, WorldToScreen(point).y);
			end
			else
			begin
				point.x := x + 0.5;
				point.y := -1.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileSlopeLeft'), WorldToScreen(point).x, WorldToScreen(point).y);
			end;
		end;

		for y := 0 to MAP_HEIGHT do
		begin
			// TileSlopeRight
			if (y mod 5 = 0) then 
			begin
				point.x := 33 + 0.5;
				point.y := y - 0.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileTunnelRight'), WorldToScreen(point).x, WorldToScreen(point).y);
			end
			else
			begin
				point.x := 33 + 0.5;
				point.y := y - 0.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileSlopeRight'), WorldToScreen(point).x, WorldToScreen(point).y);
			end;
		end;

        for x := MAP_WIDTH downto 0 do
        begin
        	for y := 0 to MAP_HEIGHT do
        	begin
        		point.x := x;
        		point.y := y;
        		if ShouldRender(WorldToScreen(point)) then
        		begin
        			DrawBitmap(game.map[x,y].bitmap, WorldToScreen(point).x, WorldToScreen(point).y);

        			if DRAW_TILE_NUMBERS then
        			begin
        				FillRectangle(ColorBlack, WorldToScreen(point).x + 15, WorldToScreen(point).y + 10, 35, 10);
        				DrawText(IntToStr(x)+','+IntToStr(y), ColorWhite, WorldToScreen(point).x + 15, WorldToScreen(point).y + 10);
        			end;
        		end;			
        	end;
        end;

        for y := 0 to MAP_HEIGHT do
        begin
        	if (y mod 5 = 0) then 
        	begin
        		point.x := x - 1;
        		point.y := y;
        		if not ShouldRender(WorldToScreen(point)) then continue;
        		DrawBitmap(BitmapNamed('TileTunnelBottomLeft'), WorldToScreen(point).x, WorldToScreen(point).y);
        	end
        	else
        	begin
        		point.x := x - 1;
        		point.y := y;
        		if not ShouldRender(WorldToScreen(point)) then continue;
        		DrawBitmap(BitmapNamed('TileSlopeBottomLeft'), WorldToScreen(point).x, WorldToScreen(point).y);
        	end;
        end;

        for x := MAP_HEIGHT downto 0 do
        begin
        	// TileSlopeRight
        	if (y mod 5 = 0) then 
        	begin
        		point.x := x;
        		point.y := 33;
        		if not ShouldRender(WorldToScreen(point)) then continue;
        		DrawBitmap(BitmapNamed('TileSlopeBottomRight'), WorldToScreen(point).x, WorldToScreen(point).y);
        	end
        	else
        	begin
        		point.x := x;
        		point.y := 33;
        		if not ShouldRender(WorldToScreen(point)) then continue;
        		DrawBitmap(BitmapNamed('TileSlopeBottomRight'), WorldToScreen(point).x, WorldToScreen(point).y);
        	end;
        end;

        // TileSlopeBottomRight
        for y := 0 to MAP_HEIGHT do
        begin
        	for x := -2 downto -22 do
        	begin
        		point.x := x + 0.5;
        		point.y := y + 0.5;
        		if not ShouldRender(WorldToScreen(point)) then continue;
        		DrawBitmap(BitmapNamed('TileConcrete'), WorldToScreen(point).x, WorldToScreen(point).y);
        	end;
        end;

		for x := MAP_WIDTH downto 0 do
		begin
			for y := 33 to 50 do
			begin
				point.x := x + 0.5;
				point.y := y + 0.5;
				if not ShouldRender(WorldToScreen(point)) then continue;
				DrawBitmap(BitmapNamed('TileConcrete'), WorldToScreen(point).x, WorldToScreen(point).y);
			end;
		end;
	end;

	// Render the agents
	procedure RenderUnits(var game : GameData);
	var
		i,j: Integer;
	begin
		for i := 0 to High(game.units) do
		begin
			if ShouldRender(game.units[i].loc) or true then
			begin
				DrawSprite(game.units[i].sprite);

				if (game.units[i].selected = true) or (game.units[i].mouseOver = true)then
				begin
					DrawBitmap('UIBldgHover', SpriteX(game.units[i].sprite), SpriteY(game.units[i].sprite) - 150);
					// FillRectangle(ColorLimeGreen, SpriteX(game.units[i].sprite), SpriteY(game.units[i].sprite) - 5, SpriteWidth(game.units[i].sprite), 6);
					// DrawRectangle(ColorBlack, SpriteX(game.units[i].sprite), SpriteY(game.units[i].sprite) - 5, SpriteWidth(game.units[i].sprite), 6);
				end;

				if DEBUG_MODE then
				begin
					// DrawLine(ColorPink, game.units[i].loc, WorldToScreen(game.units[i].destination));
					// WriteLn(PointToString(game.units[i].destination));

					for j := 1 to High(game.units[i].path) do
					begin
						// WriteLn(IntToStr(Length(game.units[i].path)));
						DrawLine(ColorWhite, WorldToScreen(game.units[i].path[j].loc), WorldToScreen(game.units[i].path[j - 1].loc));
					end;
				end;
			end;
		end;
	end;

	// Render the buildings
	procedure RenderBuildings(var game : GameData);
	var
		i,j,x,y: Integer;
		building : BuildingEntity;
		occupants : Citizens;
		shadowClr : Color;
		lwrBound, uprBound, lBound, rBound : Single;
		real : Point2D;
	begin
		for i := High(game.buildings) downto 0 do 
		begin
			if ShouldRender(SpritePosition(game.buildings[i].sprite)) then
			begin
				real := WorldToScreen(AddVectors(game.buildings[i].loc, NewPoint(1,0)));
				DrawBitmap('House01Shadow', real.x, real.y);
			end;
		end;

		for x := MAP_WIDTH downto 0 do
		begin
			for y := 0 to MAP_HEIGHT do
			begin
				if game.map[x,y].hasBuilding then
				begin
					building := game.map[x,y].building;
					// WriteLn(@building);
					if ShouldRender(SpritePosition(building.sprite)) then
					begin
						DrawSprite(building.sprite);
					end;
				end;
			end;
		end;
		// for i := High(game.buildings) downto 0 do
		// begin
		// 	if ShouldRender(SpritePosition(game.buildings[i].sprite)) then
		// 	begin
		// 		DrawSprite(game.buildings[i].sprite);
		// 	end;
		// end;

		for i := High(game.buildings) downto 0 do
		begin
			if ShouldRender(SpritePosition(game.buildings[i].sprite)) then
			begin
				if game.power < Length(game.buildings) * 30 then
					DrawBitmap('UIIconNoPower', SpriteX(game.buildings[i].sprite) + 4, SpriteY(game.buildings[i].sprite) - BitmapHeight(BitmapNamed('UIIconNoPower')));

				if (game.buildings[i].selected = true) then
				begin
					DrawBitmap('UIBldgHover', SpriteX(game.buildings[i].sprite), SpriteY(game.buildings[i].sprite) - 100);

					occupants := GetCitizensForBuilding(game, game.buildings[i].loc);

					if game.buildings[i].buildingType = RES_SMALL then
					begin
						DrawText('Residents ('+IntToStr(Length(occupants))+')', ColorBlack, 'OpenSansSemiBold16', SpriteX(game.buildings[i].sprite) + 10, SpriteY(game.buildings[i].sprite) - 96);
						for j := 0 to High(occupants) do
						begin
							DrawText(occupants[j].firstName + ' ' + occupants[j].lastName, ColorBlack, 'OpenSansRegular', SpriteX(game.buildings[i].sprite) + 10, (SpriteY(game.buildings[i].sprite) - 78) + (12*j));
						end;
					end
					else
					begin
						DrawText('Workers ('+IntToStr(Length(occupants))+')', ColorBlack, 'OpenSansSemiBold16', SpriteX(game.buildings[i].sprite) + 10, SpriteY(game.buildings[i].sprite) - 96);
					end;
				end;

				if (TimerTicks(game.buildings[i].timer) < (6*1000)) and (i = 0) then
				begin
					DrawBitmap('UINotification', SpriteX(game.buildings[i].sprite), SpriteY(game.buildings[i].sprite) - 50);
					DrawText('First Citizens!', ColorBlack, 'NotifHeader', SpriteX(game.buildings[i].sprite) + 50, SpriteY(game.buildings[i].sprite) - 45);
					DrawText('Welcome to your', ColorBlack, 'NotifContent', SpriteX(game.buildings[i].sprite) + 50, SpriteY(game.buildings[i].sprite) - 30);
					DrawText('first citizens!', ColorBlack, 'NotifContent', SpriteX(game.buildings[i].sprite) + 50, SpriteY(game.buildings[i].sprite) - 18);
				end;
			end;
		end;
	end;

	// Render the game UI
	procedure RenderUI(var game : GameData);
	var
		sideBarX, rowStartY, rowCount, i, x, y: Integer;
		sideBarPoint, sideBarBottom, miniMapPos, startTemp, endTemp, temp: Point2D;
	begin
		y := 720-64;
		if game.zoningType = TERRAIN_ZONE then
			DrawBitmap('UIZoneBgActive', ScreenToReal(Point(0,y)).x, ScreenToReal(Point(0,y)).y)
		else
			DrawBitmap('UIZoneBg', ScreenToReal(Point(0,y)).x, ScreenToReal(Point(0,y)).y);

		DrawBitmap('UIZoneRes', ScreenToReal(Point(0,y)).x, ScreenToReal(Point(0,y)).y);
		if game.zoningType = TERRAIN_COMZONE then
			DrawBitmap('UIZoneBgActive', ScreenToReal(Point(64,y)).x, ScreenToReal(Point(64,y)).y)
		else
			DrawBitmap('UIZoneBg', ScreenToReal(Point(64,y)).x, ScreenToReal(Point(64,y)).y);

		DrawBitmap('UIZoneCom', ScreenToReal(Point(64,y)).x, ScreenToReal(Point(64,y)).y);

		if game.zoningType = TERRAIN_ROAD then
			DrawBitmap('UIZoneBgActive', ScreenToReal(Point(128,y)).x, ScreenToReal(Point(128,y)).y)
		else
			DrawBitmap('UIZoneBg', ScreenToReal(Point(128,y)).x, ScreenToReal(Point(128,y)).y);

		DrawBitmap('UIZoneRoad', ScreenToReal(Point(128,y)).x, ScreenToReal(Point(128,y)).y);

		if game.zoningType = TERRAIN_POWER then
			DrawBitmap('UIZoneBgActive', ScreenToReal(Point(192,y)).x, ScreenToReal(Point(192,y)).y)
		else
			DrawBitmap('UIZoneBg', ScreenToReal(Point(192,y)).x, ScreenToReal(Point(192,y)).y);
		DrawBitmap('UIZonePower', ScreenToReal(Point(192,y)).x, ScreenToReal(Point(192,y)).y);

		if game.zoningType = TERRAIN_PLACEWATER then
			DrawBitmap('UIZoneBgActive', ScreenToReal(Point(256,y)).x, ScreenToReal(Point(256,y)).y)
		else
			DrawBitmap('UIZoneBg', ScreenToReal(Point(256,y)).x, ScreenToReal(Point(256,y)).y);
		DrawBitmap('UIZoneWater', ScreenToReal(Point(256,y)).x, ScreenToReal(Point(256,y)).y);


		if game.zoningType = TERRAIN_REMOVE then
			DrawBitmap('UIZoneBgActive', ScreenToReal(Point(SCREEN_WIDTH-64,y)).x, ScreenToReal(Point(SCREEN_WIDTH-64,y)).y)
		else
			DrawBitmap('UIZoneBg', ScreenToReal(Point(SCREEN_WIDTH-64,y)).x, ScreenToReal(Point(SCREEN_WIDTH-64,y)).y);
		DrawBitmap('UIZoneBull', ScreenToReal(Point(SCREEN_WIDTH-64,y)).x, ScreenToReal(Point(SCREEN_WIDTH-64,y)).y);
			
		temp := ScreenToReal(MousePosition());
		temp := RealToWorld(temp);
		temp := WorldBuilding(temp, BitmapNamed('PowerPlant'));
		// temp := WorldToScreen(temp);
		if game.zoningType = TERRAIN_POWER then
		begin
			DrawBitmap('PowerPlant', temp.x + 30, temp.y);
		end;

		temp := Point(MouseX() - (BitmapWidth(BitmapNamed('UIMouseSelect')) / 2), MouseY() - (BitmapHeight(BitmapNamed('UIMouseSelect')) / 2));
		temp := ScreenToReal(temp);
		if game.mouseLoc.mouseStatus = MOUSE_NORMAL then
			DrawBitmap('UIMouseSelect', temp.x, temp.y)
		else
			DrawBitmap('UIMouseMove', temp.x, temp.y);

		FillRectangle(ColorWhite, ScreenToReal(Point(0,0)).x, ScreenToReal(Point(0,0)).y, 1280, 25); // header toolbar background

		DrawText('Welcome to '+game.cityName, ColorGreen, 'HoogUILarge', ScreenToReal(Point(0,0)).x, ScreenToReal(Point(0,0)).y);

		DrawText('$', ColorGreen, 'OpenSansSemiBold22', ScreenToReal(Point(1185,0)).x, ScreenToReal(Point(1185,-3)).y);
		DrawText(IntToStr(game.money), ColorGreen, 'HoogUILarge', ScreenToReal(Point(1200,0)).x, ScreenToReal(Point(1200,0)).y);

		DrawText('Population '+IntToStr(Length(game.citizens)), ColorGreen, 'HoogUILarge', ScreenToReal(Point(1000,0)).x, ScreenToReal(Point(870,0)).y);

		DrawText('Income '+IntToStr(Length(game.citizens) * 50 - Length(game.expenses) * 500), ColorGreen, 'HoogUILarge', ScreenToReal(Point(800,0)).x, ScreenToReal(Point(600,0)).y);

		DrawText('Available Power '+IntToStr(game.power - Length(game.buildings) * 30), ColorGreen, 'HoogUILarge', ScreenToReal(Point(500,0)).x, ScreenToReal(Point(0,0)).y);

		if DEBUG_MODE then
		begin
			FillRectangle(ColorBlack, ScreenToReal(Point(0,0)).x, ScreenToReal(Point(0,30)).y , 160, 110);
			DrawText('Pop: '+IntToStr(Length(game.buildings) * 5), ColorLightGreen, ScreenToReal(Point(0,33)).x, ScreenToReal(Point(0,33)).y);
			DrawText('Bldgs: '+IntToStr(Length(game.buildings)), ColorLightGreen, ScreenToReal(Point(0,33)).x, ScreenToReal(Point(0,43)).y);
			DrawText('Zoned: '+'#', ColorLightGreen, ScreenToReal(Point(0,43)).x, ScreenToReal(Point(0,53)).y);
			DrawText('Agents: '+IntToStr(Length(game.units)), ColorLightGreen, ScreenToReal(Point(0,53)).x, ScreenToReal(Point(0,63)).y);
			DrawText('Camera: '+PointToString(CameraPos()), ColorLightGreen, ScreenToReal(Point(0,63)).x, ScreenToReal(Point(0,73)).y);
			DrawText('Mworld: '+PointToString(ScreenToWorld(MousePosition())), ColorLightGreen, ScreenToReal(Point(0,73)).x, ScreenToReal(Point(0,83)).y);
			DrawText('Mreal: '+PointToString(ScreenToReal(MousePosition())), ColorLightGreen, ScreenToReal(Point(0,83)).x, ScreenToReal(Point(0,93)).y);
		end;
	end;

	// Calls other rendering procedures
	procedure RenderGame(var game : GameData);
	begin
		ClearScreen(ColorDarkGreen);
		RenderMap(game);
		RenderUnits(game);
		RenderBuildings(game);

		HideMouse();

		RenderUI(game);

		if DEBUG_MODE then
		begin
			DrawFramerate(300,0);
		end;

		RefreshScreen();
	end;

end.
