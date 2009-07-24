#include <stdio.h>

#include "cpp-server.cpp"

int callback (TABI_ELEMENT* params)
{
	TABI_MAP p(params);
	int   n       = p._int("n");
	char *name    = p._str("name");
	char *missing = p._str("missing", "-");
	printf("%d %s %s\n", n, name, missing);
	return 0;
}

void client()
{
	server (TABI_DYNAMAP ("i",1) ("s","str") ("callback", callback));
}



main()
{
	client();
}
