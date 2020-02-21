/*
 * luastack.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <lua5.2/lua.h>
#include <lua5.2/lauxlib.h>

static void stackDump(lua_State *L)
{
	int i;
	int top = lua_gettop(L);

	for (i = 1; i <= top; ++i) { // Repeat for each stack level
		int t = lua_type(L, i);
		switch (t) {
		case LUA_TSTRING:
			printf("'%s'", lua_tostring(L, i));
			break;
		case LUA_TBOOLEAN:
			printf("%s", lua_toboolean(L, i) ? "true" : "false");
			break;
		case LUA_TNUMBER:
			printf("%g", lua_tonumber(L, i));
			break;
		default:
			printf("%s", lua_typename(L, i));
		}
		printf(" ");
	}
	printf("\n");
}

int main(int argc, char *argv[])
{
	lua_State *L = luaL_newstate();

	lua_pushboolean(L, 1);
	lua_pushnumber(L, 10);
	lua_pushstring(L, "hello");

	stackDump(L);

	lua_pushvalue(L, -1);
	stackDump(L);
	lua_remove(L, 2);
	stackDump(L);

	lua_close(L);

	return 0;
}
