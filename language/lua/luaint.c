/*
 * luaint.c
 * About: A simple Lua interpreter
 */

#include <lua5.2/lauxlib.h>
#include <lua5.2/lua.h>
#include <lua5.2/lualib.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
	char buf[256];
	int error = 0;
	// Open a new Lua VM
	lua_State *L = luaL_newstate();
	// Load all stand libraries
	luaL_openlibs(L);

	while (fgets(buf, sizeof(buf), stdin) != NULL) {
		error = luaL_loadstring(L, buf) || lua_pcall(L, 0, 0, 0);
		if (error) {
			fprintf(stderr, "%s\n", lua_tostring(L, -1));
			/* Popup the error message from the stack */
			lua_pop(L, 1);
		}
	}
	lua_close(L);

	return 0;
}
