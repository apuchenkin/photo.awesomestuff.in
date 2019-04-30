import { Entity, PrimaryGeneratedColumn, Column, OneToMany } from "typeorm";
import { PageTranslation } from './translation';
import { Language } from "../translation";

@Entity()
export class Page {
  @PrimaryGeneratedColumn()
  id: number;

  @Column({
    unique: true,
  })
  alias: string;

  @Column({
    default: true,
  })
  hidden: boolean;

  @OneToMany(() => PageTranslation, translation => translation.page, { cascade: true })
  translations: PageTranslation[];

  languages: Language[];
}

export default Page;

