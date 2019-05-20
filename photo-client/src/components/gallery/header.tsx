import * as React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Link from 'found/lib/Link';
import style from '@app/components/page/header.scss';

interface Props {
  category: Category;
  active: Category;
}

const messages = defineMessages({
  home: {
    id: 'home',
    defaultMessage: 'Home',
  },
});

const GalleryHeader: React.FunctionComponent<Props> = ({ category }) => (
  <header className={style.main}>
    <h1 className={style.title}>
      <Link to="/"><FormattedMessage {...messages.home} /></Link>
      <> / </>
      <Link to={`/${category.name}`}>{category.title}</Link>
    </h1>
  </header>
);

export default withStyles(style)(GalleryHeader);
