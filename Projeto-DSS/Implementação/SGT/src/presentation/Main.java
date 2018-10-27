package presentation;

import business.Facade;

/**
 * Classe Main.
 * É aqui que todo o programa é inicializado.
 * A Main trata de iniciar uma nova instância de Facade e uma 
 * nova instância de LoginFrame, chamando este último.
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Facade f = new Facade();
        LoginFrame lf = new LoginFrame(f);
        
        lf.setLocationRelativeTo(null);
        lf.setVisible(true);
    }
}
