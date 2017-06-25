import React from 'react';
import { string, element, shape } from 'prop-types';
import withRouter from 'found/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Loader from './loader/loader';
import Footer from './footer';

const Ps = isBrowser ? require('perfect-scrollbar') : null;

class Main extends React.PureComponent {
  componentDidMount() {
    const me = this;

    Ps.initialize(me.content);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  render() {
    const { data: { className, header }, children } = this.props;

    return (
      <div className={[style.main, style[className]].join(' ')}>
        {React.createElement(header)}
        <div className={style.content} ref={(c) => { this.content = c; }}>
          {children}
          <Footer />
        </div>
        <Loader key="loader" />
      </div>
    );
  }
}

Main.propTypes = {
  data: shape({
    className: string.isRequired,
    header: element.isRequired,
  }).isRequired,
  children: element.isRequired,
};

export default withStyles(style, baseStyle)(
  withRouter(Main),
);
