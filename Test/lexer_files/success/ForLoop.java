public class ForLoop
{
	public static void main(String[] args)
	{	
		int k = 0;
		for(;;) {
			System.out.println(k);
			if (k > 1) {
				break;
			}
			k++;
		}

		k = 0;
		for(;; k++) {
			System.out.println(k);
			if (k > 1) {
				break;
			}
		}

		k = 0;
		for(; k < 2;) {
			System.out.println(k);
			k++;
		}

		k = 0;
		for(; k < 2; k++) {
			System.out.println(k);
		}

		for(k = 0;;) {
			System.out.println(k);
			if (k > 1) {
				break;
			}
			k++;
		}

		for(k = 0;; k++) {
			System.out.println(k);
			if (k > 1) {
				break;
			}
		}

		for(k = 0; k < 2;) {
			System.out.println(k);
			k++;
		}

		for(k = 0; k < 2; k++) {
			System.out.println(k);
		}

		for(k = 0; k < 2; k++)
			System.out.println(k);
	}
}
