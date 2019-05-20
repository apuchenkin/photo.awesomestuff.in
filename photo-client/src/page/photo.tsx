import * as React from 'react';
import { Helmet } from 'react-helmet';
import { injectIntl, defineMessages, InjectedIntl } from 'react-intl';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import Figure from '@app/components/photo/figure';
import Link from 'found/lib/Link';
import IconLeft from '@app/components/photo/icons/left';
import IconRight from '@app/components/photo/icons/right';

import style from './photo.scss';
import { Router } from 'found';

const messages = defineMessages({
  prev: {
    id: 'prev',
    defaultMessage: 'Prev',
  },
  next: {
    id: 'next',
    defaultMessage: 'Next',
  },
});

interface Props {
  category: Category,
  parent?: Category,
  photos: Photo[],
  photo: Photo,
  router: Router,
  intl: InjectedIntl,
}

const Photo: React.FunctionComponent<Props> = ({
  intl,
  category,
  parent,
  photos,
  photo,
  router,
}) => {
  const pidx = photos.findIndex(p => p.id === photo.id);
  const categoryPath = [parent && parent.name, category.name].filter(Boolean).join('/');
  const prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1];
  const next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1];
  const backUrl = `/${categoryPath}`;
  const onClick = React.useCallback(
    (e: React.MouseEvent) => {
      router.push(`${backUrl}/photo/${next.id}`);
      e.stopPropagation();
    },
    [backUrl, next.id]
  );

  const onClose = React.useCallback(
    () => router.push(backUrl),
    [backUrl]
  );

  return (
    // eslint-disable-next-line jsx-a11y/no-static-element-interactions
    <div className={style.photo} onClick={onClose}>
      <Helmet>
        <title>{photo.description}</title>
        <meta
          name="description"
          content={intl.formatMessage({ id: 'meta.description.photo' }, {
            author: photo.author,
            title: photo.description,
          })}
        />
      </Helmet>
      <Figure
        photo={photo}
        backUrl={backUrl}
        onClick={onClick}
      />
      <Link
        to={`${backUrl}/photo/${prev.id}`}
        title={intl.formatMessage(messages.prev)}
        className={`${style.nav} ${style.prev}`}
        onClick={event => event.stopPropagation()}
      >
        <IconLeft />
      </Link>
      <Link
        to={`${backUrl}/photo/${next.id}`}
        className={`${style.nav} ${style.next}`}
        title={intl.formatMessage(messages.next)}
        onClick={event => event.stopPropagation()}
      >
        <IconRight />
      </Link>
    </div>
  );
}

export default withStyles(style)(injectIntl(Photo));
