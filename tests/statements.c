int main()
{
	int x;
	for (int i = 0; i < 20; i++) {
		if (i % 3 == 0)
			x = 23;
		else if (i % 5 == 0)
			x = 45;
		else 
			x = 34;
	}

	return x;
}
