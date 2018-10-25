/**
 * Classe Actor (Objeto principal da aplicação).
 * 
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

import java.io.*;
import java.util.Calendar;

public class Actor implements Serializable {
    /** Variáveis de Instância */
    private String email;
    private String nome;
    private String password;
    private String morada;
    private Calendar dataDeNascimento;
    
    /** 
     * Construtor vazio.
     */
    public Actor() {
        this.email = "";
        this.nome = "";
        this.password = "";
        this.morada = "";
        this.dataDeNascimento = dataDeNascimento.getInstance();  
    }
    
    /** 
     * Construtor por cópia.
     * @param a
     */
    public Actor(Actor a) {
        this.email = a.getEmail();
        this.nome = a.getNome();
        this.password = a.getPassword();
        this.morada = a.getMorada();
        this.dataDeNascimento = a.getData();
    }   
    
    /** 
     * Construtor por parâmetro.
     * @param email
     * @param nome
     * @param password
     * @param morada
     * @param dataDeNascimento
     */
    public Actor(String email, String nome, String password, String morada, Calendar dataDeNascimento) {
        this.email = email;
        this.nome = nome;
        this.password = password;
        this.morada = morada;
        this.dataDeNascimento = dataDeNascimento;
    }
    
    /**
     * Getters e Setters da classe Actor.
     */
    public String getEmail() { 
        return this.email; 
    }
    
    public String getNome() {
        return this.nome;
    }
    
    public String getPassword() {
        return this.password;
    }
    
    public String getMorada() {
        return this.morada;
    }
    
    public Calendar getData() {
        return this.dataDeNascimento;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }
    
    public void setNome(String nome) {
        this.nome = nome;
    }
    
    public void setPassword(String password) {
        this.password = password;
    }
    
    public void setMorada(String morada) {
        this.morada = morada;
    }
    
    public void setData(Calendar data) {
        this.dataDeNascimento = data;
    }
    
    /**
     * Compara igualdade com outro objeto.
     * @param o
     * @return
     */
    public boolean equals (Object o) {
        if (this == o) return true;
        
        if ((o == null) || (this.getClass() != o.getClass())) return false;
        
        Actor aux = (Actor) o;
        return (this.email.equals(aux.getEmail()) && this.nome.equals(aux.getNome()) && 
                this.password.equals(aux.getPassword()) && this.morada.equals(aux.getMorada()) &&
                this.dataDeNascimento.equals(aux.getData()));
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return
     */
    public String toString() {
        String aux = new String();
        aux = ("Actor: \n" + 
               "Nome: " + this.nome + "\n" + 
               "Password: " + this.password + "\n" + 
               "Morada: " + this.morada + "\n" +
               "Data de Nascimento: " + this.dataDeNascimento.get(Calendar.DAY_OF_MONTH) + "/" + 
                                       (this.dataDeNascimento.get(Calendar.MONTH) + 1) + "/" + 
                                        this.dataDeNascimento.get(Calendar.YEAR));
            
        return aux;
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return
     */
    public Actor clone() {
        return new Actor(this);
    }
}
