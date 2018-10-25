/**
 * Classe Carrinhas (subclasse de Taxi).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.util.List;
import java.util.ArrayList;

public class Carrinhas extends Taxi {

    /** 
     * Construtor vazio.
     */
    public Carrinhas() {
        super();
    }
    
    /** 
     * Construtor por cópia.
     * @param cl
     */ 
    public Carrinhas(Carrinhas cl) {
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
    public Carrinhas(String matricula, Motorista condutor, double velMedia, double precoKm, double factorFiabilidade, 
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
        return new String("-> Carrinha: \n" + super.toString());
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
                
        Carrinhas cl = (Carrinhas) umCarro;
        return(super.equals(cl));      
    } 
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */
    public Carrinhas clone() {
        return new Carrinhas(this);
    }
}
