import React from 'react';
import { string, element, shape, func } from 'prop-types';

import withStyles from 'isomorphic-style-loader/lib/withStyles';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Loader from '../components/loader';
import Footer from '../components/footer';

const Ps = isBrowser ? require('perfect-scrollbar') : null;

class Main extends React.PureComponent {
  componentDidMount() {
    Ps.initialize(this.content);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  componentWillUnmount() {
    Ps.destroy(this.content);
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
    className: string,
    header: func.isRequired,
  }).isRequired,
  children: element.isRequired,
};

export default withStyles(style, baseStyle)(Main);
