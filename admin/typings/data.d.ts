interface Timestamp {
  createdAt: Date;
  updatedAt: Date;
}

interface Category extends Timestamp {
  id: number;
  name: string;
  title: string;
  description?: string;
  hidden: boolean;
  date?: Date;
  parent?: Category
  featured?: string;
  translations: Translation[];
  children: Category[];
}

interface Photo {
  id: number;
  src: string;
  views: number;
  width: number;
  height: number;
  group?: number;
  description?: string;
  author?: string;
}

interface Translation {
  language: string;
  field: string;
  value: string;
}

interface Page extends Timestamp {
  alias: string;
  title: string;
  content:string;
  hidden: boolean;
  translations: Translation[];
  description?: string;
  langs: Locale[];
}

type Locale = "ru" | "en"
