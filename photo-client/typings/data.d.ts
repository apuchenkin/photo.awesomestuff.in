interface Timestamp {
  createdAt: Date;
  updatedAt: Date;
}

interface Category extends Timestamp {
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
  datetime: Date;
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

interface GalleryConfig {
  width: number;
  height: number;
}

interface Config {
  hostname: string;
  analytics: string | false;
  title: string;
  locales: Locale[];
  fallbackLocale: Locale;
  gutter: number;
  brickWidth: number;
  transition: 125;
  gallery: GalleryConfig;
  photo: GalleryConfig;
}