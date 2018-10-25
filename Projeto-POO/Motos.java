/**
 * Classe Motos (subclasse de Taxi).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.util.List;
import java.util.ArrayList;
public class Motos extends Taxi {
    
    /** 
     * Construtor vazio.
     */
    public Motos() {
        super();
    }
    
    /** 
     * Construtor por cópia.
     * @param cl
     */ 
    public Motos(Motos cl) {
        super(cl);
    }
    
    /** 
     * Construtor por parâmetro.
     * @param matricula
     * @param condutor
     * @param velMedia
     * @param precoKm
     * @param factorFiabilidade
     * @param totalFaturado
     * @param coords
     * @param viagensEfetuadas
     */
    public Motos(String matricula, Motorista condutor, double velMedia, double precoKm, double factorFiabilidade, 
                 double totalFaturado, Ponto2D coords, ArrayList<Viagem> viagensEfetuadas) {
        super(matricula, condutor, velMedia, precoKm, factorFiabilidade, totalFaturado, coords, viagensEfetuadas);
    }
     
    /**
     * Determina o preço por Km do veículo.
     * @return
     */
    public double preco() {
        return this.getPrecoKm()*10;
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */ 
    public String toString() {
        return new String("-> Motos: \n" + super.toString());
    } 
    
    /**
     * Compara igualdade com outro objeto.
     * @param umCarro
     * @return
     */ 
    public boolean equals(Object umCarro) {
        if (this == umCarro) 
            return true;
        
        if ((umCarro == null || (this.getClass() != umCarro.getClass())))
            return false;
                 
        Motos cl = (Motos) umCarro;
        return(super.equals(cl));      
    } 
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */ 
    public Motos clone() {
        return new Motos(this);
    }
}
