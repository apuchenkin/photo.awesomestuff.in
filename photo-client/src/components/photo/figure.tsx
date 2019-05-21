import * as React from 'react';
import { defineMessages, FormattedMessage } from 'react-intl';
import Link from 'found/lib/Link';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
// @ts-ignore
import Image from 'react-image';
import Loader from '@app/components/loader';
import Close from './icons/close';
import style from './figure.scss';

const messages = defineMessages({
  close: {
    id: 'icon.close',
    defaultMessage: 'Close',
  },
  author: {
    id: 'photo.author',
    defaultMessage: 'Author: {author}',
  },
});

interface ToolsProps {
  backUrl: string;
}

const Tools: React.FunctionComponent<ToolsProps> = ({ backUrl }) => (
  <div className={style.tools}>
    <Link onClick={e => e.stopPropagation()} to={backUrl}>
      <FormattedMessage
        {...messages.close}
      /><Close />
    </Link>
  </div>
)

interface CaptionProps {
  description?: string;
  author?: string;
}

const Caption: React.FunctionComponent<CaptionProps> = ({ author, description }) => (
  <figcaption className={style.description}>
    <span className={style.caption}>{description}</span>
    {author && (
      <div>
        <FormattedMessage
          {...messages.author}
          values={{ author: (<span className={style.author}>{author}</span>) }}
        />
      </div>
    )}
  </figcaption>
)

interface Props {
  photo: Photo;
  backUrl: string;
  onClick: React.MouseEventHandler;
}

const Figure: React.FunctionComponent<Props> = ({ photo, backUrl, onClick }) => (
  <figure className={style.figure}>
    <Tools backUrl={backUrl} />
    <img
      onClick={onClick}
      src={`/static/photo/${photo.src}`}
      alt={photo.description}
      className={style.image}
    />
    <Caption author={photo.author} description={photo.description} />
  </figure>
);

export default withStyles(style)(Figure);
