class Hello
{
    int g; // lol comment
    
    void main()
    {
        test(5, 6);
    }

    void test(int x, int y)
    {
        5 + 5;
        int b;
        6 + 6;
    }
    
    int square(int x)
    {
        int y;
        y = x * x;
        return y;   
    }

    int abs(int x)
    {
    	
        if (x < 0)
            x = 0 - x;
        return x;
    }
    
    int fac(int x)
    {
        int r; int t;
        t = 1; r = 1;
        while (t <= x)
        {
            r = r * t;
            t = t + 1;
        }
        return r;
   }
}
