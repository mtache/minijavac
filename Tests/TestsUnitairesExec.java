public class A {
    int attr;
    public static void main() {
        int x = 1;
        int d = 4;
        float f = 1.5 /0.75;
        float z = 3 * (4 / 1.25);
        boolean a = true && false;
        boolean b = true && true;
        boolean c = true || false;
        int e = x + d;
         boolean p = d < x;
         boolean comp1 = 5.2< 5;
         boolean comp2 = 5 <= 5.1;
         boolean comp3 = 5 > 4;
         boolean comp4 = 5.5 >= 3 && true;

         if(x<5) {
           x += 1;
         }
         int whiler = 0;

         while(whiler<5) {
           whiler+=4  ;
         }
         e = e + 1;
         e += 1;
         
         for (int i =0; i < 5; i = i +1) {
            d += 1;
         }
         //deuxième exemple de for
        int n = 1 ;
        int v=1;
        for (n; (n < 10) && true ; n = n +2) {
            v += 1;
         }
         
    }

    public void m2() {

    }
}
