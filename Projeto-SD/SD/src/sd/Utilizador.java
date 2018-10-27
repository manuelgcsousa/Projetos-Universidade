package sd;

public class Utilizador {

	private String username;
	private String password;
	private int rank;

	public Utilizador (String username, String password, int rank) {
		this.username = username;
		this.password = password;
		this.rank = rank;
	}

	public String getUsername() {
		return this.username;
	}

	public String getPassword() {
		return this.password;
	}

	public int getRank() {
		return this.rank;
	}

	public void setUsername (String username) {
		this.username = username;
	}

	public void setPassword (String password) {
		this.password = password;
	}

	public void setRank (int rank) {
		this.rank = rank;
	}

	public String toString() {
		return "-> Jogador: @" + this.username + "\n" +
                       "-> Password: $" + this.password + "\n" +
                       "-> Rank: " + this.rank;
	}
}
