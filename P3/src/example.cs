class Hello
{
    int g; // lol comment
    
    void main()
    {
        int b;
        b = 1;
    }
    
    int square(int x)
    {
        int y;
        y = x * x;
        return /* this is not x */ y;   
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
