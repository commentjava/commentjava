// Found on: https://stackoverflow.com/questions/21224266/java-syntax-error-on-token
public class Hmwk {
        public static void main(String[] args){
                public static double footToMeter(double foot){
                        return 0.305 * foot;
                }
                public static double meterToFoot(double meter){
                        return 3.279 * meter;
                }
                for (double i = 1.0; i <11; i++){
                        System.out.printf(i+footToMeter(i)+"|"+(i*5+15)+meterToFoot(i*5+15));
                }
        }
}

