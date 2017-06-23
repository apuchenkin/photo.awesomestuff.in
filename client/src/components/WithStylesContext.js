import React from 'react';
import { element, func } from 'prop-types';

class WithStylesContext extends React.PureComponent {
  getChildContext() {
    return { insertCss: this.props.onInsertCss };
  }

  render() {
    return React.Children.only(this.props.children);
  }
}

WithStylesContext.propTypes = {
  children: element.isRequired,
  onInsertCss: func.isRequired,
};

WithStylesContext.childContextTypes = {
  insertCss: func.isRequired,
};

export default WithStylesContext;
