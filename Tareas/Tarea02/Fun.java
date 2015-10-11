/* -------------------------------------------------------------------
 * Fun.java
 * versión 1.0
 * Copyright (C) 2015  José Ricardo Rodríguez Abreu.
 * Facultad de Ciencias,
 * Universidad Nacional Autónoma de México, Mexico.
 *
 * Este programa es software libre; se puede redistribuir
 * y/o modificar en los términos establecidos por la
 * Licencia Pública General de GNU tal como fue publicada
 * por la Free Software Foundation en la versión 2 o
 * superior.
 *
 * Este programa es distribuido con la esperanza de que
 * resulte de utilidad, pero SIN GARANTÍA ALGUNA; de hecho
 * sin la garantía implícita de COMERCIALIZACIÓN o
 * ADECUACIÓN PARA PROPÓSITOS PARTICULARES. Véase la
 * Licencia Pública General de GNU para mayores detalles.
 *
 * Con este programa se debe haber recibido una copia de la
 * Licencia Pública General de GNU, de no ser así, visite el
 * siguiente URL:
 * http://www.gnu.org/licenses/gpl.html
 * o escriba a la Free Software Foundation Inc.,
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * -------------------------------------------------------------------
 */

/**
 * @author Jose Ricardo Rodriguez Abreu
 * @version 1.0
 * @since Oct 11 2015.
 */

public class Fun{

    /**
     * Función que siempre regresa false.
     * @return false.
     */
    public static boolean fFalse(){
	System.out.println("Entró a fFalse");
	return false;
    }        
    
    /**
     * Función que siempre regresa true.
     * @return true.
     */
    public static boolean fTrue(){
	System.out.println("Entró a fTrue");
	return true;
    }
            
    /**
     * Método factorial.
     * @param n - es el entero a sacar factorial.
     * @return el factorial de n.
     */
    public static int fac(int n){
	if(n == 0)
	    return 1;
	return (n*fac(n-1));
    }

    /**
     * Método principal de la clase.
     * @param args - son los parámetros que recibe el programa.
     */
    public static void main(String[] args){
	if(args.length != 0)
	    fac(10000000);
	if(false && (20 == fac(100000000)))
	    System.out.println("20 es el factorial de 100000000");
	if(true || (20 == fac(100000000)))
	    System.out.println("No revisó factorial");
	boolean a = fFalse();
	fTrue();
	if(fTrue() || !a)
	    System.out.println("Evaluó ambos!");
    }    
} //Fin de Fun.java
