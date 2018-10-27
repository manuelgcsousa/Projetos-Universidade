namespace CleanicaFinal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    [Table("Funcionario")]
    public partial class Funcionario
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public Funcionario()
        {
            Formulario_de_Realizacao = new HashSet<Formulario_de_Realizacao>();
            Formulario_de_Satisfacao = new HashSet<Formulario_de_Satisfacao>();
            ServicosRealizados = new HashSet<ServicosRealizado>();
        }

        [Key]
        [StringLength(50)]
        public string Utilizador_email { get; set; }

        public int Servico_id_servico { get; set; }

        public int cc { get; set; }

        public int telemovel { get; set; }

        [Required]
        [StringLength(25)]
        public string IBAN { get; set; }

        [Column(TypeName = "date")]
        public DateTime birthdate { get; set; }

        [Required]
        [StringLength(100)]
        public string morada { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Formulario_de_Realizacao> Formulario_de_Realizacao { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Formulario_de_Satisfacao> Formulario_de_Satisfacao { get; set; }

        public virtual Utilizador Utilizador { get; set; }

        public virtual Servico Servico { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<ServicosRealizado> ServicosRealizados { get; set; }
    }
}
