class Hello
{
    int g; // lol comment
    
    void main()
    {
        bool bo;
        bo = true;
    }

    int test(int a)
    {
        int b;
        if (a == 10)
        {
            int c;
            c = 6;
            b = 5;
        }
        else
        {
            b = 9;
        }
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
