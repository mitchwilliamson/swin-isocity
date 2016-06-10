//
// IsoCity - Isometric City Builder
// Written by Mitchell Williamson
// April-June 2016
//
//=============================================================================
// GameMain.pas
//=============================================================================
//
// The core game file, calls all other units and maintains the game variables
//
//=============================================================================

program GameMain;
uses SwinGame, sgTypes, gInput, gMap, gTypes, gRender, gResources, gUnit, gUnitAI, gGUI, Math;

procedure Main();
var
	game: GameData;
	x,y, tX, tY,idx, i : Integer;
	pos : Point2D;
begin
	DebugMsg('Game starting in debug mode...');
	DebugMsg('----- Isometric City Builder -----');
	DebugMsg('-- Written by Mitchell Williamson --');
	DebugMsg('Init: Starting Game Initialising...');

	InitResources(game); // Load the resources...
	InitRendering(); // Start the rendering...
	Randomize(); // Seed randomness

	GenerateMap(game); // Generate the game map
	game.mouseLoc.mouseStatus := MOUSE_NORMAL;
	game.power := 0; // Set the base power
	game.money := 50000; // Set the base money
	game.guiStatus := STARTUP_MODE;
	game.menuStatus := MAIN_SCREEN;
	game.zoningType := TERRAIN_ZONE;

	DebugMsg('Init: Game Init Complete.');

	PlayMusic('MainThemeOne'); // Play the game music

	if SHOW_SPLASHSCREEN then
		ShowSwinGameSplashScreen(); // Play the SwinGame intro splashscreen

	if game.guiStatus = MAIN_MENU then
	begin
		DebugMsg('UI: Starting Menu Rendering...');
		repeat
			ProcessEvents();
			DrawMainMenu(game);
		until (WindowCloseRequested()) or (game.guiStatus <> MAIN_MENU);
		DebugMsg('UI: Stopping Menu Rendering...');
	end;

	if game.guiStatus = IN_GAME then
	begin
		MoveCameraTo(Point(340,-340));
		DebugMsg('Init: Starting Game Rendering...');
		repeat;
			ProcessEvents();
			HandleInput(game); // gInput.pas
			AITick(game); // gUnitAI.pas
			RenderGame(game); // gRender.pas

			// Manage income
			if TimerTicks(game.moneyTimer) > (PAY_TIME*1000) then
			begin
				ResetTimer(game.moneyTimer);
				StartTimer(game.moneyTimer);
				game.money += Length(game.citizens) * 50;
				PlaySoundEffect('PaidNotif');
			end;
		until WindowCloseRequested();
		DebugMsg('Init: Stopping Game Rendering...');
	end;

	DebugMsg('Resources: Releasing Resources...');
	ReleaseAllResources();
	DebugMsg('Resources: Releasing Resources... done.');
	DebugMsg('--Game Shutdown--')
end;

begin
	Main();
end.
