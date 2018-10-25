/** 
 * Classe Contributor guarda a informação de um contribuidor.
 *
 * É guardado o ID de user;
 * O username do contribuidor;
 * O número total de vezes que esse contribuidor editou artigos.
 *
 * @author André Vieira
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

package engine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Contributor {
	/**
     * Variáveis de Instância.
     */
    private String userID;
	private String username;
	private int reps;
    
    /**
     * Construtor vazio.
     */
	public Contributor() {
		this.userID = "";
		this.username = "";
		this.reps = 0;
	}
    
    /**
     * Construtor por parâmetro.
     * 
     * @param userID
     * @param username
     * @param reps
     */
	public Contributor (String userID, String username, int reps) {
		this.userID = userID;
		this.username = username;
		this.reps = reps;
	}
    
    /**
     * Construtor por cópia.
     * 
     * @param art
     */
	public Contributor (Contributor art) {
		this.userID = art.getUserID();
		this.username = art.getUsername();
		this.reps = art.getReps();
	}
    
    /**
     * Conjunto de Getters e Setters da classe Contributor.
     */
	public String getUserID() {
		return this.userID;
	}

	public String getUsername() {
		return this.username;
	}

	public int getReps() {
		return this.reps;
	}

	public void setUserID (String userID) {
		this.userID = userID;
	}

	public void setUsername (String username) {
		this.username = username;
	}

	public void setReps (int reps) {
		this.reps = reps;
	}
    
    /** 
     * Devolve uma representação da classe Contributor em modo textual.
     * 
     * @return String
     */
	public String toString() {
		return ("ID do Contribuidor: " + this.userID +  
		        "\nUsername: " + this.username + "\nRepetições: " + this.reps);
	}
    
    /** 
     * Compara igualdade com outro objeto.
     * 
     * @param o
     * @return boolean
     */
	public boolean equals (Object o) {
        if (this == o) return true;
        
        if ((o == null) || (this.getClass() != o.getClass())) return false;
        
        Contributor aux = (Contributor) o;
        
        return (this.userID.equals(aux.getUserID()) && 
        		this.username.equals(aux.getUsername()) && this.reps == aux.getReps());
    }
    
    /** 
     * Retorna uma cópia da instância.
     * 
     * @return Contributor
     */
    public Contributor clone() {
    	return new Contributor(this);
    }
 }
