namespace CleanicaFinal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    [Table("ServicosRealizados")]
    public partial class ServicosRealizado
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public ServicosRealizado()
        {
            Funcionarios = new HashSet<Funcionario>();
        }

        [Key]
        public int id_ServicosRealizados { get; set; }

        [Required]
        [StringLength(50)]
        public string Cliente_Utilizador_email { get; set; }

        [Required]
        [StringLength(100)]
        public string morada { get; set; }

        public DateTime data { get; set; }

        public float preco { get; set; }

        [Required]
        [StringLength(1)]
        public string dimensao { get; set; }

        [Required]
        [StringLength(1)]
        public string prods { get; set; }

        [StringLength(150)]
        public string aspectosimp { get; set; }

        [Required]
        [StringLength(1)]
        public string pendente { get; set; }

        public int duracao { get; set; }

        public virtual Cliente Cliente { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public virtual ICollection<Funcionario> Funcionarios { get; set; }
    }
}
