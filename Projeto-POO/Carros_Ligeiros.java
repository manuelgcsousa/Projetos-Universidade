/**
 * Classe Carros_Ligeiros (subclasse de Taxi).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.util.List;
import java.util.ArrayList;

public class Carros_Ligeiros extends Taxi {
    
    /** 
     * Construtor vazio.
     */
    public Carros_Ligeiros() {
        super();
    }
    
    /** 
     * Construtor por cópia.
     * @param cl
     */  
    public Carros_Ligeiros(Carros_Ligeiros cl) {
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
    public Carros_Ligeiros(String matricula, Motorista condutor, double velMedia, double precoKm, double factorFiabilidade, 
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
        return new String("-> Carros Ligeiros: \n" + super.toString());
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
                 
        Carros_Ligeiros cl = (Carros_Ligeiros) umCarro;
        return(super.equals(cl));      
    } 
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */ 
    public Carros_Ligeiros clone() {
        return new Carros_Ligeiros(this);
    }
}
